CREATE TABLE template_schema_relationship(
  id SERIAL NOT NULL PRIMARY KEY,
  name VARCHAR(255) NOT NULL,
  display_name VARCHAR(255) NOT NULL,
  description VARCHAR(255) NOT NULL,
  schema JSONB,
  from_id VARCHAR(255) NOT NULL REFERENCES template_schema(id) ON DELETE CASCADE,
  to_id VARCHAR(255) NOT NULL REFERENCES template_schema(id) ON DELETE CASCADE,
  created_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP
)
