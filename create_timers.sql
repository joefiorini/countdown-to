-- Table: public.timers

-- DROP TABLE public.timers;
CREATE SEQUENCE public.timers_id_seq
    INCREMENT 1
    START 2
    MINVALUE 1
    MAXVALUE 2147483647
    CACHE 1;

ALTER SEQUENCE public.timers_id_seq
    OWNER TO joe;
CREATE TABLE public.timers
(
    id integer NOT NULL DEFAULT nextval('timers_id_seq'::regclass),
    description text COLLATE pg_catalog."default" NOT NULL,
    short text COLLATE pg_catalog."default" NOT NULL,
    starts_on timestamp with time zone NOT NULL,
    ends_on timestamp with time zone NOT NULL,
    CONSTRAINT timers_pkey PRIMARY KEY (id),
    CONSTRAINT timers_short_key UNIQUE (short)
)
WITH (
    OIDS = FALSE
)
TABLESPACE pg_default;

ALTER TABLE public.timers
    OWNER to joe;

-- Index: timers_short_index

-- DROP INDEX public.timers_short_index;

CREATE INDEX timers_short_index
    ON public.timers USING btree
    (short COLLATE pg_catalog."default")
    TABLESPACE pg_default;