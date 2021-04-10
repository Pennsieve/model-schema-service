ALTER TABLE template_schema_relationship ADD COLUMN deleted BOOLEAN NOT NULL DEFAULT false;
ALTER TABLE template_schema_linked_property ADD COLUMN deleted BOOLEAN NOT NULL DEFAULT false;
ALTER TABLE dataset_template ADD COLUMN deleted BOOLEAN NOT NULL DEFAULT false;
