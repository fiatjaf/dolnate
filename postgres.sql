CREATE TABLE donations (
  id serial PRIMARY KEY,
  email text NOT NULL,
  note text NOT NULL,
  amt int NOT NULL,
  domain text NOT NULL
);

CREATE TABLE withdrawals (

);
