USE TestDB;

CREATE TABLE dbo.Inventory
(id INT,
name NVARCHAR (50),
quantity INT,
PRIMARY KEY (id));


INSERT INTO dbo.Inventory
VALUES (1, 'banana', 150);

INSERT INTO dbo.Inventory
VALUES (2, 'orange', 154);
