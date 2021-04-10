CREATE TABLE dataset_template(
  id SERIAL NOT NULL PRIMARY KEY,
  dataset_id INTEGER NOT NULL,
  organization_id INTEGER NOT NULL,
  name VARCHAR(255) NOT NULL,
  description VARCHAR(255) NOT NULL,
  created_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
  updated_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP
);

CREATE TRIGGER dataset_template_update_updated_at BEFORE UPDATE ON dataset_template FOR EACH ROW EXECUTE PROCEDURE update_updated_at_column();
