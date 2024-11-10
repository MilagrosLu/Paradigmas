object inmobiliaria {
    const operacionesRealizadas =[]
    var property porcentajeVentaComision = 1.5
    const empleados = []
    method mejorEmpleadoSegun(criterio) = criterio.apply(empleados)
    method agregarOperacion (operacion) {
        operacionesRealizadas.add(operacion)
    }
}
class CriterioPorMaximo {
    const condicion
    method apply (empleados) = empleados.max{empleado => condicion.evaluar(empleado)}
}
object condicionComisionOperacionesCerradas {
    method evaluar(empleado) =  empleado.totalComisionGanada()
}
object condicionCantidadOperacionesCerradas {
    method evaluar(empleado) = empleado.cantidadOperacionesCerradas()
}
object condicionCantidadReservas {
    method evaluar(empleado) =  empleado.cantidadReservas()
}