--BADANIE BAZY NORTHWIND


--Analiza rentownoœci produktów na podstawie kategorii i dostawców
SELECT c.CategoryName, s.CompanyName AS Supplier, p.ProductName, 
       SUM(od.Quantity * od.UnitPrice) AS TotalRevenue,
       SUM(od.Quantity * (od.UnitPrice - od.Discount)) AS NetRevenue
FROM Products p
JOIN Categories c ON p.CategoryID = c.CategoryID
JOIN Suppliers s ON p.SupplierID = s.SupplierID
JOIN [Order Details] od ON p.ProductID = od.ProductID
GROUP BY c.CategoryName, s.CompanyName, p.ProductName
ORDER BY NetRevenue DESC;



--Analiza najlepiej sprzedaj¹cych siê produktów w ramach konkretnego dostawcy
WITH ProductTotalSales AS (
    SELECT 
        od.ProductID,
        SUM(od.Quantity * od.UnitPrice * (1 - od.Discount)) AS TotalSales
    FROM [Order Details] od
    GROUP BY od.ProductID
)
SELECT 
    s.CompanyName AS SupplierName,
    p.ProductName, 
    SUM(od.Quantity * od.UnitPrice * (1 - od.Discount)) AS TotalSales
FROM Suppliers s
JOIN Products p ON s.SupplierID = p.SupplierID
JOIN [Order Details] od ON p.ProductID = od.ProductID
JOIN ProductTotalSales pts ON pts.ProductID = p.ProductID
GROUP BY s.CompanyName, p.ProductName
HAVING SUM(od.Quantity * od.UnitPrice * (1 - od.Discount)) > (
    SELECT AVG(TotalSales)
    FROM ProductTotalSales
)
ORDER BY SupplierName, TotalSales DESC;



--Jakie s¹ przychody z poszczególnych produktów w ró¿nych krajach, u ró¿nych dostawców, w ró¿nych kategoriach?
SELECT cu.Country, s.CompanyName AS SupplierName, c.CategoryName, p.ProductName, 
       SUM(od.Quantity * od.UnitPrice) AS TotalRevenue
FROM Orders o
JOIN Customers cu ON o.CustomerID = cu.CustomerID
JOIN [Order Details] od ON o.OrderID = od.OrderID
JOIN Products p ON od.ProductID = p.ProductID
JOIN Categories c ON p.CategoryID = c.CategoryID
JOIN Suppliers s ON p.SupplierID = s.SupplierID
GROUP BY cu.Country, s.CompanyName, c.CategoryName, p.ProductName
ORDER BY cu.Country, SupplierName, CategoryName, TotalRevenue DESC;


