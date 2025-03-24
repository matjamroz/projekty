# wprowadzenie kolumny marża
select * from products p 

ALTER TABLE products ADD COLUMN Decimal_product_cost DECIMAL(10,2),
add column Decimal_product_price DECIMAL(10,2);
UPDATE products 
SET Decimal_product_cost = CAST(REPLACE(Product_Cost, '$', '') AS DECIMAL(10,2)),
 Decimal_product_price = CAST(REPLACE(Product_price, '$', '') AS DECIMAL(10,2));





# Które kategorie produktów przynoszą największe zyski
with tabela as (
    select 
        (p.Decimal_product_price - p.Decimal_product_cost) as marza,
        p.Product_ID,
        p.Product_Category,
        s.Units
    from products p 
    left join sales s on s.Product_ID = p.Product_ID
)
select 
    SUM(marza * Units) as Gain, 
    Product_Category
from tabela
group by Product_Category
order by SUM(marza * Units);

# Czy istnieje korelacja z lokalizacją?
with tabela as (
    select 
        p.Product_Category,
        st.Store_Location,
        SUM(
            (p.Decimal_product_price - p.Decimal_product_cost) * COALESCE(s.Units, 0)
        ) AS total_profit
    from products p
    left join sales s on s.Product_ID = p.Product_ID
    left join stores st on s.Store_ID = st.Store_ID
    group by p.Product_Category, st.Store_Location
)
select 
    Store_Location,
    Product_Category,
    total_profit
from tabela
order by total_profit,Store_Location;


# Jaka jest korelacja między poziomem zapasów a sprzedażą w różnych lokalizacjach?
select
    Sa.Store_ID,
    St.Store_Name,
    I.Product_ID,
    P.Product_Name,
    I.Stock_On_Hand,
    SUM(Sa.Units) AS Total_Sales
from
    Sales Sa
join
    Inventory I ON Sa.Store_ID = I.Store_ID AND Sa.Product_ID = I.Product_ID
join
    Products P ON Sa.Product_ID = P.Product_ID
join
    Stores St ON Sa.Store_ID = St.Store_ID
group by
    Sa.Store_ID,
    St.Store_Name,
    I.Product_ID,
    P.Product_Name,
    I.Stock_On_Hand
order by 
     Sa.Store_ID ,I.Stock_On_Hand;
   
# Jakie sklepy miały wyższą marżę w porównaniu do średniej marży wszystkich sklepów w 2022?
SELECT
    St.Store_ID,
    St.Store_Name,
    St.Store_City,
    St.Store_Location,
    SUM(Sa.Units * (P.Decimal_product_price - P.Decimal_product_cost)) AS Gain
FROM
    Sales Sa
JOIN
    Products P ON Sa.Product_ID = P.Product_ID
JOIN
    Stores St ON Sa.Store_ID = St.Store_ID
WHERE
    Sa.Date BETWEEN '2022-01-01' AND '2022-12-31'
GROUP BY
    St.Store_ID, St.Store_Name, St.Store_City, St.Store_Location
HAVING
    SUM(Sa.Units * (P.Decimal_product_price - P.Decimal_product_cost)) >
    (
        SELECT AVG(StoreMargin)
        FROM (
            SELECT
                Su.Store_ID,
                SUM(Su.Units * (P2.Decimal_product_price - P2.Decimal_product_cost)) AS StoreMargin
            FROM Sales Su
            JOIN Products P2 ON Su.Product_ID = P2.Product_ID
            WHERE Su.Date BETWEEN '2022-01-01' AND '2022-12-31'
            GROUP BY Su.Store_ID
        ) as tmp
    )
ORDER BY
    St.Store_City, St.Store_Location;
