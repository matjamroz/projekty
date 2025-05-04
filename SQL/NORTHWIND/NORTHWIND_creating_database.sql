select SERVERPROPERTY('productversion'),
SERVERPROPERTY('edition')

USE orders_dwh;
GO

-- Wymiar: Klient
CREATE TABLE Customers (
    CustomerID NVARCHAR(5) PRIMARY KEY,
    CompanyName NVARCHAR(100),
    City NVARCHAR(50),
    Country NVARCHAR(50)
);

-- Wymiar: Produkt
CREATE TABLE Products (
    ProductID INT PRIMARY KEY,
    ProductName NVARCHAR(100),
    CategoryName NVARCHAR(100)
);

-- Tabela faktów
CREATE TABLE Order_facts (
    OrderID INT,
    CustomerID NVARCHAR(5),
    ProductID INT,
    OrderDate DATE,
    Quantity INT,
    UnitPrice MONEY,
    PRIMARY KEY (OrderID, ProductID) 
);

select * from Products

-- Klienci
INSERT INTO Customers (CustomerID, CompanyName, City, Country)
SELECT distinct CustomerID, CompanyName, City, Country
FROM NORTHWND.dbo.Customers;

-- Produkty z kategoriami
INSERT INTO Products (ProductID, ProductName, CategoryName)
SELECT P.ProductID, P.ProductName, C.CategoryName
FROM NORTHWND.dbo.Products P
JOIN NORTHWND.dbo.Categories C ON P.CategoryID = C.CategoryID;

-- Fakty: Zamówienia
INSERT INTO Order_facts (OrderID, CustomerID, ProductID, OrderDate, Quantity, UnitPrice)
SELECT 
    O.OrderID,
    O.CustomerID,
    OD.ProductID,
    O.OrderDate,
    OD.Quantity,
    OD.UnitPrice
FROM NORTHWND.dbo.Orders O
JOIN NORTHWND.dbo.[Order Details] OD ON O.OrderID = OD.OrderID;

select * from Order_facts
select * from Customers
DELETE FROM Order_facts
WHERE CustomerID NOT IN (SELECT CustomerID FROM Customers);
-- Relacja z Customers
ALTER TABLE Order_facts
ADD CONSTRAINT FK_OrderFacts_Customers
FOREIGN KEY (CustomerID) REFERENCES Customers(CustomerID);

-- Relacja z Products
ALTER TABLE Order_facts
ADD CONSTRAINT FK_OrderFacts_Products
FOREIGN KEY (ProductID) REFERENCES Products(ProductID);

SELECT 
    f.name AS ForeignKey,
    OBJECT_NAME(f.parent_object_id) AS TableName,
    COL_NAME(fc.parent_object_id,fc.parent_column_id) AS ColumnName,
    OBJECT_NAME (f.referenced_object_id) AS RefTableName,
    COL_NAME(fc.referenced_object_id,fc.referenced_column_id) AS RefColumnName
FROM 
    sys.foreign_keys AS f
INNER JOIN 
    sys.foreign_key_columns AS fc 
    ON f.object_id = fc.constraint_object_id;

SELECT name, state_desc FROM sys.databases;