CREATE TABLE threads (
  id INTEGER PRIMARY KEY NOT NULL,
  by TEXT NOT NULL,
  content TEXT NOT NULL,
  parent_id INTEGER NULL,
  at TEXT NOT NULL,
  FOREIGN KEY(parent_id) REFERENCES threads(id)
);
