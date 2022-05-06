type NombreDelProducto = String
type PrecioDelProducto = Float
type DiaDeEntrega = String

type Producto = (NombreDelProducto, PrecioDelProducto, DiaDeEntrega)

-- funciones para las tuplas
tomarNombre:: Producto-> NombreDelProducto
tomarNombre (nombreDelProducto,_, _)= nombreDelProducto

tomarPrecio:: Producto->PrecioDelProducto
tomarPrecio (_, precioDelProducto, _)= precioDelProducto

tomarDiaDeEntrega:: Producto->DiaDeEntrega
tomarDiaDeEntrega (_, _, diaDeEntrega)= diaDeEntrega

hacerProducto :: String->Float->String->Producto
hacerProducto nombre precio dia=(nombre, precio, dia) 
---------------

aplicarDescuento:: Num a =>a->Producto->a
aplicarDescuento  descuento producto= tomarPrecio producto - ((tomarPrecio producto)*descuento/100)

aplicarCostoDeEnvio:: Num a => Producto->a->a
aplicarCostoDeEnvio producto costoDeEnvio= (tomarPrecio producto)+costoDeEnvio

precioTotal:: Num a=> Producto->a->a->a->
precioTotal producto cantidad descuento costoDeEnvio= (aplicarCostoDeEnvio producto costoDeEnvio)+((*cantidad).(aplicarDescuento descuento) $producto)     

entregaSencilla:: Producto->Bool
entregaSencilla producto = even.length $tomarDiaDeEntrega 

descodiciarProducto:: Producto->Producto
descodiciarProducto producto= hacerProducto ((take 10).tomarNombre $producto) (tomarPrecio producto) (tomarDiaDeEntrega producto)

productoDeLujo:: Producto->Bool
productoDeLujo producto= elem 'x' (tomarNombre producto) || elem 'z' (tomarNombre producto)

productoCodiciado::Producto->Bool
productoCodiciado producto= (tomarNombre producto) >10

productoCorriente:: Producto-> Bool
productoCorriente producto= elem (head.tomarNombre $producto) "aeiouAEIOU"

productoDeElite:: Producto->Bool
productoDeElite producto = not(productoCorriente producto) && productoDeLujo producto && productoCodiciado producto

productoXL:: Producto->Producto
productoXL producto= hacerProducto ((tomarNombre producto)++"XL") (tomarPrecio producto) (tomarDiaDeEntrega producto)

versionBarata:: Producto->Producto
productoDeElite producto= reverse.tomarNombre.descodiciarProducto $producto
