precio_base_seguro::Float->Float
precio_base_seguro importe = if importe < 0 then 0 else 0.01 * importe

porcentaje_tipo_poliza::String->Float
porcentaje_tipo_poliza codigo = case codigo of 
                                            "TB" -> 20
                                            "TC" -> 50
                                            "TR" -> 100
                                            otherwise -> 75

monto_tipo_poliza::Float->String->Float
monto_tipo_poliza importe tipo = ((precio_base_seguro importe) * (porcentaje_tipo_poliza tipo)) / 100

es_0Km::Float->Bool
es_0Km km = km < 20
--es_0Km km = if km < 20 then True else False

monto_0Km::Float->Float
monto_0Km km = if es_0Km km then 5000 else 0

porcentaje_comision::String->Float
porcentaje_comision codigo = case codigo of
                                         "ven_01" -> 5
                                         "ven_02" -> 6
                                         "ven_03" -> 6.5
                                         otherwise -> -1

precio_final_seguro::Float->String->Float->Float
precio_final_seguro importe codigo km = precio_base_seguro importe + monto_tipo_poliza importe codigo + monto_0Km km

lista_comisiones::[String]->[Float]->[Float]
lista_comisiones [] [] = []
lista_comisiones (c:cs) (p:ps) = if length cs == length ps then (((porcentaje_comision c) * p)/100):lista_comisiones cs ps else []

calcular_porcentaje::Float->Float->Float
calcular_porcentaje porcentaje importe = porcentaje * importe / 100

comision_vendedor::String->[Float]->Float
comision_vendedor _ [] = 0
comision_vendedor codigo (p:ps) = if porc == -1 then -1 else (calcular_porcentaje porc p)+comision_vendedor codigo ps
                                  where porc = porcentaje_comision codigo

lista_vehiculos_rechazados:: [(String, Int)] -> [String]
lista_vehiculos_rechazados [] = []
lista_vehiculos_rechazados (t:ts) = if snd t > 20 then fst t:lista_vehiculos_rechazados ts else lista_vehiculos_rechazados ts           