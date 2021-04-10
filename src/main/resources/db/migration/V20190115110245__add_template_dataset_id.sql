ALTER TABLE template_schema ADD COLUMN dataset_template_id INTEGER;
ALTER TABLE template_schema ADD CONSTRAINT fk_dataset_template_id FOREIGN KEY (dataset_template_id) REFERENCES dataset_template(id);