create table reminders (
  id UUID PRIMARY KEY,
  description VARCHAR(500) NOT NULL,
  creation_date DATE NOT NULL
);