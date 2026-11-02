/*
PROJECT: Flight Performance & Delay Analysis
DATABASE: PostgreSQL
SOURCE TABLE: public.flights

BUSINESS QUESTIONS:
1. Which airlines perform best and worst?
2. Which airports have the highest flight traffic?
3. How has flight performance changed over time?
4. When are flights most likely to be delayed?

OUTPUT:
Four SQL views for Power BI.
*/


-- ============================================================
-- 00. DATA EXPLORATION AND VALIDATION
-- ============================================================

-- Total number of imported rows
SELECT COUNT(*) AS total_rows
FROM public.flights;


-- Inspect the source data
SELECT *
FROM public.flights
LIMIT 10;


-- Number of airlines and airports
SELECT COUNT(DISTINCT reporting_airline) AS number_of_airlines
FROM public.flights;

SELECT COUNT(DISTINCT origin) AS number_of_origin_airports
FROM public.flights;


-- Check year coverage
SELECT
    year,
    COUNT(*) AS total_flights
FROM public.flights
GROUP BY year
ORDER BY year;


-- Verify the values used for cancellation and delay flags
SELECT DISTINCT cancelled
FROM public.flights
ORDER BY cancelled;

SELECT DISTINCT arrdel15
FROM public.flights
ORDER BY arrdel15;


-- Check missing values in important columns
SELECT
    COUNT(*) AS total_rows,

    COUNT(*) FILTER (
        WHERE reporting_airline IS NULL
        OR TRIM(reporting_airline) = ''
    ) AS missing_airline,

    COUNT(*) FILTER (
        WHERE origin IS NULL
        OR TRIM(origin) = ''
    ) AS missing_origin,

    COUNT(*) FILTER (
        WHERE dest IS NULL
        OR TRIM(dest) = ''
    ) AS missing_destination,

    COUNT(*) FILTER (
        WHERE arrdelayminutes IS NULL
        OR TRIM(arrdelayminutes) = ''
    ) AS missing_arrival_delay

FROM public.flights;


-- ============================================================
-- 01. AIRLINE PERFORMANCE
-- Business question: Which airlines perform best and worst?
-- View: vw_airline_performance
-- ============================================================

CREATE OR REPLACE VIEW public.vw_airline_performance AS

WITH airline_performance AS (
    SELECT
        op_unique_carrier AS airline_code,

        COUNT(*) AS total_flights,

        ROUND(
            AVG(
                CASE
                    WHEN TRIM(cancelled) = '0'
                    THEN NULLIF(TRIM(arrdelayminutes), '')::NUMERIC
                END
            ), 2
        ) AS avg_arrival_delay_minutes,

        SUM(CASE WHEN TRIM(cancelled) = '1' THEN 1 ELSE 0 END)
            AS cancelled_flights,

        SUM(
            CASE
                WHEN TRIM(cancelled) = '0'
                AND TRIM(arrdel15) = '1'
                THEN 1 ELSE 0
            END
        ) AS delayed_flights,

        SUM(CASE WHEN TRIM(cancelled) = '0' THEN 1 ELSE 0 END)
            AS completed_flights

    FROM public.flights

    WHERE op_unique_carrier IS NOT NULL
      AND TRIM(op_unique_carrier) <> ''

    GROUP BY op_unique_carrier
)

SELECT
    airline_code,
    total_flights,
    completed_flights,
    cancelled_flights,
    delayed_flights,
    avg_arrival_delay_minutes,

    ROUND(
        cancelled_flights * 100.0 / NULLIF(total_flights, 0), 2
    ) AS cancellation_rate_pct,

    ROUND(
        delayed_flights * 100.0 / NULLIF(completed_flights, 0), 2
    ) AS delay_rate_pct,

    RANK() OVER (
        ORDER BY avg_arrival_delay_minutes ASC NULLS LAST
    ) AS best_average_delay_rank,

    RANK() OVER (
        ORDER BY delayed_flights * 100.0
        / NULLIF(completed_flights, 0) ASC NULLS LAST
    ) AS best_delay_rate_rank

FROM airline_performance;


-- ============================================================
-- 02. AIRPORT RANKINGS
-- Business question: Which airports have the highest traffic?
-- View: vw_airport_rankings
-- ============================================================

CREATE OR REPLACE VIEW public.vw_airport_rankings AS

WITH airport_traffic AS (

    -- Departure traffic by airport
    SELECT
        origin AS airport_code,
        origincityname AS airport_city,
        'Departure' AS flight_type,
        COUNT(*) AS total_flights

    FROM public.flights

    WHERE TRIM(cancelled) = '0'
      AND origin IS NOT NULL
      AND TRIM(origin) <> ''

    GROUP BY
        origin,
        origincityname

    UNION ALL

    -- Arrival traffic by airport
    SELECT
        dest AS airport_code,
        destcityname AS airport_city,
        'Arrival' AS flight_type,
        COUNT(*) AS total_flights

    FROM public.flights

    WHERE TRIM(cancelled) = '0'
      AND dest IS NOT NULL
      AND TRIM(dest) <> ''

    GROUP BY
        dest,
        destcityname
),

airport_rankings AS (

    SELECT
        airport_code,
        airport_city,
        flight_type,
        total_flights,

        RANK() OVER (
            PARTITION BY flight_type
            ORDER BY total_flights DESC
        ) AS airport_rank

    FROM airport_traffic
)

SELECT *
FROM airport_rankings;


-- ============================================================
-- 03. MONTHLY FLIGHT PERFORMANCE
-- Business question: How has flight performance changed over time?
-- View: vw_monthly_performance
-- ============================================================

CREATE OR REPLACE VIEW public.vw_monthly_performance AS

WITH monthly_performance AS (

    SELECT
        year,
        month,

        COUNT(*) AS total_flights,

        SUM(
            CASE
                WHEN TRIM(cancelled) = '0'
                THEN 1
                ELSE 0
            END
        ) AS completed_flights,

        SUM(
            CASE
                WHEN TRIM(cancelled) = '1'
                THEN 1
                ELSE 0
            END
        ) AS cancelled_flights,

        -- Average arrival delay for completed flights
        AVG(
            CASE
                WHEN TRIM(cancelled) = '0'
                THEN NULLIF(
                    TRIM(arrdelayminutes), ''
                )::NUMERIC
            END
        ) AS avg_arrival_delay_minutes,

        -- Flights delayed by at least 15 minutes
        SUM(
            CASE
                WHEN TRIM(cancelled) = '0'
                AND TRIM(arrdel15) = '1'
                THEN 1
                ELSE 0
            END
        ) AS delayed_flights

    FROM public.flights

    GROUP BY
        year,
        month
)

SELECT
    year,
    month,
    total_flights,
    completed_flights,
    cancelled_flights,
    delayed_flights,

    ROUND(
        avg_arrival_delay_minutes,
        2
    ) AS avg_arrival_delay_minutes,

    -- Cancellation percentage
    ROUND(
        cancelled_flights * 100.0
        / NULLIF(total_flights, 0),
        2
    ) AS cancellation_rate_pct,

    -- Delay percentage among completed flights
    ROUND(
        delayed_flights * 100.0
        / NULLIF(completed_flights, 0),
        2
    ) AS delay_rate_pct

FROM monthly_performance;


-- ============================================================
-- 04. FLIGHT DELAY PATTERNS
-- Business question: When are flights most likely to be delayed?
-- View: vw_delay_patterns
-- ============================================================

CREATE OR REPLACE VIEW public.vw_delay_patterns AS

WITH delay_patterns AS (

    SELECT
        month,
        dayofweek,
        deptimeofday,

        COUNT(*) AS completed_flights,

        SUM(
            CASE
                WHEN TRIM(arrdel15) = '1'
                THEN 1
                ELSE 0
            END
        ) AS delayed_flights,

        AVG(
            NULLIF(
                TRIM(arrdelayminutes), ''
            )::NUMERIC
        ) AS avg_arrival_delay_minutes

    FROM public.flights

    WHERE TRIM(cancelled) = '0'

    GROUP BY
        month,
        dayofweek,
        deptimeofday
)

SELECT
    month,
    dayofweek,
    deptimeofday,
    completed_flights,
    delayed_flights,

    ROUND(
        avg_arrival_delay_minutes,
        2
    ) AS avg_arrival_delay_minutes,

    ROUND(
        delayed_flights * 100.0
        / NULLIF(completed_flights, 0),
        2
    ) AS delay_rate_pct

FROM delay_patterns;


-- ============================================================
-- 05. VALIDATE THE FOUR VIEWS
-- ============================================================

-- Airline performance
SELECT *
FROM public.vw_airline_performance
ORDER BY best_average_delay_rank;


-- Top 10 departure and arrival airports
SELECT *
FROM public.vw_airport_rankings
WHERE airport_rank <= 10
ORDER BY flight_type, airport_rank;


-- Monthly performance
SELECT *
FROM public.vw_monthly_performance
ORDER BY year, month;


-- Delay patterns
SELECT *
FROM public.vw_delay_patterns
ORDER BY delay_rate_pct DESC;