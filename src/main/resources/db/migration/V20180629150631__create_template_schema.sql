CREATE TABLE template_schema(
  id VARCHAR(255) NOT NULL PRIMARY KEY,
  org_id INTEGER NOT NULL,
  schema VARCHAR(255) NOT NULL,
  name VARCHAR(255) NOT NULL,
  display_name VARCHAR(255) NOT NULL,
  description TEXT,
  category VARCHAR(255),
  properties JSONB,
  required JSONB,
  created_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP
)
