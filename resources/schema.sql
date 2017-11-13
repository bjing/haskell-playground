CREATE SEQUENCE words_id_seq;

CREATE TABLE words (
    id          integer PRIMARY KEY DEFAULT nextval('words_id_seq'),
    word        varchar(50),
    definition  Varchar(200)
);

ALTER SEQUENCE words_id_seq
OWNED BY words.id;
