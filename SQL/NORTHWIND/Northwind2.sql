--Praca na bazie danych Northwind w MSSQL
--Liczba zam�wie� klient�w w latach 1996�1998 
SELECT  
  CustomerID, 
  COUNT(CASE WHEN YEAR(OrderDate) = 1996 THEN 1 END) AS Orders1996, 
  COUNT(CASE WHEN YEAR(OrderDate) = 1997 THEN 1 END) AS Orders1997, 
  COUNT(CASE WHEN YEAR(OrderDate) = 1998 THEN 1 END) AS Orders1998 
FROM Orders 
GROUP BY CustomerID; 

--Pracownicy z min. 2 zam�wieniami do kraju na "S" i nigdy <2 produkt�w w zam. 
SELECT e.LastName, e.FirstName 
FROM Employees e 
JOIN Orders o ON e.EmployeeID = o.EmployeeID 
WHERE o.ShipCountry LIKE 'S%' 
GROUP BY e.EmployeeID, e.LastName, e.FirstName 
HAVING COUNT(DISTINCT o.OrderID) >= 2 
AND NOT EXISTS ( 
  SELECT 1 
  FROM Orders o2 
  JOIN [Order Details] od2 ON o2.OrderID = od2.OrderID 
  WHERE o2.EmployeeID = e.EmployeeID 
  GROUP BY o2.OrderID 
  HAVING SUM(od2.Quantity) < 2 
); 
--Pracownicy, kt�rzy sprzedali produkt >110% �redniej jego zamawianej ilo�ci 
WITH ProductAverageSales AS ( 
    SELECT p.ProductID, 
        AVG(od.Quantity) AS AvgQuantitySold 
    FROM [Order Details] od 
    JOIN Products p ON od.ProductID = p.ProductID 
    GROUP BY p.ProductID 
), 
EmployeeProductSales AS ( 
    SELECT e.EmployeeID, 
        e.FirstName, 
        e.LastName, 
        od.ProductID, 
        SUM(od.Quantity) AS TotalQuantitySold 
    FROM Employees e 
    JOIN Orders o ON e.EmployeeID = o.EmployeeID 
    JOIN [Order Details] od ON o.OrderID = od.OrderID 
    GROUP BY e.EmployeeID, e.FirstName, e.LastName, od.ProductID 
) 
SELECT 
    eps.EmployeeID, 
    eps.FirstName, 
    eps.LastName, 
    p.ProductName, 
    eps.TotalQuantitySold 
FROM EmployeeProductSales eps 
JOIN ProductAverageSales pas ON eps.ProductID = pas.ProductID 
JOIN Products p ON eps.ProductID = p.ProductID 
WHERE eps.TotalQuantitySold >= 1.10 * pas.AvgQuantitySold 
ORDER BY eps.LastName, eps.FirstName, p.ProductName;

--Liczba sprzedanych produkt�w wg kategorii (rolling sum 3 mies. w 1997) 
WITH MonthlySales AS ( 
  SELECT  
    c.CategoryName, 
    CAST(YEAR(o.OrderDate) AS VARCHAR) + '-' + RIGHT('0' + CAST(MONTH(o.OrderDate) AS VARCHAR), 2) AS Period, 
    YEAR(o.OrderDate) AS y, 
    MONTH(o.OrderDate) AS m, 
    SUM(od.Quantity) AS Qty 
  FROM Orders o 
  JOIN [Order Details] od ON o.OrderID = od.OrderID 
  JOIN Products p ON od.ProductID = p.ProductID 
  JOIN Categories c ON p.CategoryID = c.CategoryID 
  WHERE YEAR(o.OrderDate) = 1997 
  GROUP BY c.CategoryName, YEAR(o.OrderDate), MONTH(o.OrderDate) 
), 
Rolling AS ( 
  SELECT *, SUM(Qty) OVER (PARTITION BY CategoryName ORDER BY m ROWS BETWEEN 2 PRECEDING AND CURRENT ROW) AS RollingQty 
  FROM MonthlySales 
)
SELECT Period, CategoryName, Qty AS TotalCountForMonth, RollingQty AS TotalCountForLast3Months 
FROM Rolling 
ORDER BY Period, CategoryName; 

--Lista pracownik�w o nazwisku nie zawieraj�cym liter 'o' oraz 'a', posortowanych po nazwisku w kolejno�ci malej�cej. 
SELECT FirstName, LastName 
FROM Employees 
WHERE LastName NOT LIKE '%o%' AND LastName NOT LIKE '%a%' 
ORDER BY LastName DESC; 

--Lista zam�wie� z kwot� powy�ej �redniej dla zam�wie� zawieraj�cych produkt 'Boston Crab Meat'. 
WITH OrderTotals AS ( 
    SELECT OrderID, SUM(Quantity * UnitPrice) AS TotalValue 
    FROM [Order Details] 
    GROUP BY OrderID 
), 
BostonOrders AS ( 
    SELECT DISTINCT o.OrderID 
    FROM Orders o 
    JOIN [Order Details] od ON o.OrderID = od.OrderID 
    JOIN Products p ON od.ProductID = p.ProductID 
    WHERE p.ProductName = 'Boston Crab Meat' 
) 
SELECT o.OrderID, o.OrderDate, ot.TotalValue 
FROM Orders o 
JOIN OrderTotals ot ON o.OrderID = ot.OrderID 
WHERE o.OrderID IN (SELECT OrderID FROM BostonOrders) AND ot.TotalValue > (SELECT AVG(TotalValue) FROM OrderTotals) 
ORDER BY ot.TotalValue DESC; 

--Lista produkt�w, dla kt�rych ��czna zam�wiona ilo�� jest wy�sza w okresie maj-pa�dziernik ni� listopad-kwiecie�. 
SELECT p.ProductName, 
	SUM(CASE WHEN MONTH(o.OrderDate) BETWEEN 5 AND 10 THEN od.Quantity ELSE 0 END) AS MayToOct, 
	SUM(CASE WHEN MONTH(o.OrderDate) IN (11, 12, 1, 2, 3, 4) THEN od.Quantity ELSE 0 END) AS NovToApr 

FROM [Order Details] od 
JOIN Orders o ON od.OrderID = o.OrderID 
JOIN Products p ON od.ProductID = p.ProductID 
GROUP BY p.ProductName 
HAVING 
	SUM(CASE WHEN MONTH(o.OrderDate) BETWEEN 5 AND 10 THEN od.Quantity ELSE 0 END) > 
    SUM(CASE WHEN MONTH(o.OrderDate) IN (11, 12, 1, 2, 3, 4) THEN od.Quantity ELSE 0 END) 
ORDER BY MayToOct DESC; 

--Lista zam�wie� zawieraj�cych razem co najmniej 5 r�nych produkt�w, kt�re nie by�y nigdy dostarczone oraz kt�re nie by�y nigdy dostarczone do Niemiec.  
SELECT o.OrderID 
FROM Orders o 
JOIN [Order Details] od ON o.OrderID = od.OrderID 
JOIN Products p ON od.ProductID = p.ProductID 
WHERE o.ShipCountry <> 'Germany' AND o.OrderID NOT IN ( 
        SELECT DISTINCT OrderID 
        FROM [Order Details] 
		WHERE Quantity = 0 ) 
GROUP BY o.OrderID 
HAVING COUNT(DISTINCT p.ProductID) >= 5 
ORDER BY o.OrderID; 

--Liczba zam�wie� z�o�onych w kolejnych miesi�cach roku 1997, wyliczona nast�pnie od pocz�tku roku oraz w okresie bie��cego i poprzedzaj�cego dw�ch miesi�cy. 
WITH MonthlyOrders AS ( 
    SELECT  
        YEAR(OrderDate) AS OrderYear, 
        MONTH(OrderDate) AS OrderMonth, 
        COUNT(*) AS OrdersInMonth 
    FROM Orders 
    WHERE YEAR(OrderDate) = 1997 
    GROUP BY YEAR(OrderDate), MONTH(OrderDate) 
) 
SELECT  
    OrderMonth, 
    OrdersInMonth, 
    SUM(OrdersInMonth) OVER (ORDER BY OrderMonth ROWS BETWEEN UNBOUNDED PRECEDING AND CURRENT ROW) AS CumulativeFromStart, 
    SUM(OrdersInMonth) OVER (ORDER BY OrderMonth ROWS BETWEEN 2 PRECEDING AND CURRENT ROW) AS Rolling3Months 
FROM MonthlyOrders 
ORDER BY OrderMonth 

 

--Wypisz wszystkie nazwy produkt�w zawieraj�ce w sobie liter� "o", kt�re kiedykolwiek zosta�y wys�ane do Kanady, a nigdy nie wys�ane do Niemiec. Wyniki uszereguj rosn�co. 



SELECT DISTINCT p.ProductName 
FROM Products p 
JOIN [Order Details] od ON p.ProductID = od.ProductID 
JOIN Orders o ON od.OrderID = o.OrderID 
WHERE p.ProductName LIKE '%o%' AND o.ShipCountry = 'Canada' AND o.OrderID NOT IN ( 
        SELECT OrderID 
        FROM Orders 
        WHERE ShipCountry = 'Germany' 
    ) 
ORDER BY p.ProductName; 

 

--Wypisz nazwiska wszystkich pracownik�w, kt�rzy sprzedali przynajmniej 10 sztuk (quantity) produktu o nazwie 'Chang'. 

SELECT DISTINCT e.LastName 
FROM Employees e 
JOIN Orders o ON e.EmployeeID = o.EmployeeID 
JOIN [Order Details] od ON o.OrderID = od.OrderID 
JOIN Products p ON od.ProductID = p.ProductID 
WHERE p.ProductName = 'Chang' AND od.Quantity >= 10 
ORDER BY e.LastName; 

--Wypisz nazwy kategorii tych produkt�w, kt�re by�y sprzedawane cz�ciej w drugiej po�owie 1997 roku ni� w pierwszej po�owie 1998 roku. 
SELECT c.CategoryName 
FROM Categories c 
JOIN Products p ON c.CategoryID = p.CategoryID 
JOIN [Order Details] od ON p.ProductID = od.ProductID 
JOIN Orders o ON od.OrderID = o.OrderID 
WHERE (o.OrderDate BETWEEN '1997-07-01' AND '1997-12-31') OR (o.OrderDate BETWEEN '1998-01-01' AND '1998-06-30') 
GROUP BY c.CategoryName 
HAVING 
    SUM(CASE WHEN o.OrderDate BETWEEN '1997-07-01' AND '1997-12-31' THEN od.Quantity ELSE 0 END) > 
    SUM(CASE WHEN o.OrderDate BETWEEN '1998-01-01' AND '1998-06-30' THEN od.Quantity ELSE 0 END); 

--Dla ka�dego klienta znajd� nazw� produktu, kt�rego zam�wi� w najmniejszej ilo�ci (quantity). 

SELECT c.CompanyName, p.ProductName, MIN(od.Quantity) AS MinQuantity 
FROM Customers c 
JOIN Orders o ON c.CustomerID = o.CustomerID 
JOIN [Order Details] od ON o.OrderID = od.OrderID 
JOIN Products p ON od.ProductID = p.ProductID 
GROUP BY c.CompanyName, p.ProductName 
ORDER BY c.CompanyName; 

-- Zam�wienia z�o�one w dni robocze z kosztem dostawy > 100 lub w pierwszym tygodniu miesi�ca przez pracownika o ID 4 lub 6. 

SELECT 
    o.OrderID, 
    SUM(od.Quantity * od.UnitPrice) AS TotalOrderValue 
FROM Orders o 
JOIN [Order Details] od ON o.OrderID = od.OrderID 
WHERE (DATENAME(dw, o.OrderDate) IN ('Monday', 'Tuesday', 'Wednesday', 'Thursday', 'Friday') AND o.Freight > 100) OR (DAY(o.OrderDate) BETWEEN 1 AND 7 AND o.EmployeeID IN (4, 6)) 
GROUP BY o.OrderID 
ORDER BY  TotalOrderValue DESC; 

-- Lista kontynent�w z liczb� produkt�w dostarczanych z konkretnego kontynentu. 
SELECT 
    CASE 
        WHEN s.Country IN ('USA', 'Canada') THEN 'North America' 
        WHEN s.Country IN ('Japan', 'Singapore') THEN 'Asia' 
        ELSE 'Other' 
    END AS supplier_continent, 
    COUNT(p.ProductID) AS product_count 
FROM Suppliers s 
JOIN Products p ON s.SupplierID = p.SupplierID 
GROUP BY 
    CASE 
        WHEN s.Country IN ('USA', 'Canada') THEN 'North America' 
        WHEN s.Country IN ('Japan', 'Singapore') THEN 'Asia' 
        ELSE 'Other' 
    END 
ORDER BY supplier_continent; 

--Lista klient�w, kt�rzy z�o�yli zam�wienia poza przedzia�em 01 stycznia 1997 - 01 czerwca 1997, z adresami zawieraj�cymi 'rue' i imi� kontaktowe spe�niaj�ce okre�lone warunki. 
SELECT DISTINCT 
    c.CustomerID, 
    c.CompanyName, 
    c.ContactName, 
    c.Address 
FROM Customers c 
JOIN Orders o ON c.CustomerID = o.CustomerID 
WHERE o.OrderDate NOT BETWEEN '1997-01-01' AND '1997-06-01' 
    AND c.Address LIKE '%rue%' AND (c.ContactName LIKE '[A-F]%' OR SUBSTRING(c.ContactName, 3, 1) = 'N'); 

--Lista pracownik�w z informacj� o ��cznej warto�ci ich sprzeda�y, liczbie obs�u�onych zam�wie�, pozycji w rankingu sprzeda�y oraz podziale na kwartyle. 
WITH EmployeeSales AS ( 
    SELECT e.EmployeeID, 
        e.FirstName, 
        e.LastName, 
        SUM(od.Quantity * od.UnitPrice) AS TotalSalesValue, 
        COUNT(DISTINCT o.OrderID) AS OrderCount, 
        DENSE_RANK() OVER (ORDER BY SUM(od.Quantity * od.UnitPrice) DESC) AS SalesRank, 
        NTILE(4) OVER (ORDER BY SUM(od.Quantity * od.UnitPrice) DESC) AS SalesQuartile 
    FROM Employees e 
    JOIN Orders o ON e.EmployeeID = o.EmployeeID 
    JOIN [Order Details] od ON o.OrderID = od.OrderID
    GROUP BY e.EmployeeID, e.FirstName, e.LastName 
) 
SELECT 
    EmployeeID, 
    FirstName, 
    LastName, 
    TotalSalesValue, 
    OrderCount, 
    SalesRank, 
    SalesQuartile 
FROM  EmployeeSales 
ORDER BY TotalSalesValue DESC; 

--Dane zgrupowane wed�ug kraj�w dostawc�w z obliczeniami: ��czna liczba zam�wionych produkt�w, �rednia cena produktu oraz maksymalna warto�� pojedynczego zam�wienia. 
SELECT s.Country, COUNT(od.ProductID) AS TotalProductsOrdered, AVG(p.UnitPrice) AS AverageProductPrice, MAX(od.Quantity * od.UnitPrice) AS MaxSingleOrderValue 
FROM Suppliers s 
JOIN Products p ON s.SupplierID = p.SupplierID 
JOIN [Order Details] od ON p.ProductID = od.ProductID 
GROUP BY s.Country 
ORDER BY s.Country; 

 