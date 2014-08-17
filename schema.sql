CREATE TABLE threads (
  id INTEGER PRIMARY KEY,
  by TEXT,
  content TEXT,
  parent_id INTEGER,
  at TEXT,
  FOREIGN KEY(parent_id) REFERENCES threads(id)
);

INSERT INTO threads VALUES (0, 'anon', 'Basilica', 0, datetime('now'));
