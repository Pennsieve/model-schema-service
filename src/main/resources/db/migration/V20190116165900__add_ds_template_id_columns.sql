ALTER TABLE template_schema_relationship ADD COLUMN dataset_template_id INTEGER;
ALTER TABLE template_schema_relationship ADD CONSTRAINT fk_dataset_template_id FOREIGN KEY (dataset_template_id) REFERENCES dataset_template(id);

ALTER TABLE template_schema_linked_property ADD COLUMN dataset_template_id INTEGER;
ALTER TABLE template_schema_linked_property ADD CONSTRAINT fk_dataset_template_id FOREIGN KEY (dataset_template_id) REFERENCES dataset_template(id);
