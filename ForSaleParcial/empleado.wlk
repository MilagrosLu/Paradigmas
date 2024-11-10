import operaciones.*
class Empleado {
    var ingreso
    const reservas 
    const operacionesCerradas
    method cobrarComision (operacion) {
        ingreso += operacion.comision()
    }
    method pedirReserva(operacion, cliente)  {
        self.verificarPosibilidadReserva(operacion)
        operacion.reservarse(cliente)
        reservas.add(operacion)

    }
    method verificarPosibilidadReserva(operacion) {
        if(operacion.reservada()) {
            throw new DomainException (message = "Ya fue reservada")
        }
    }
    method concretarOperacion(operacion, cliente) {
        self.verificarReserva(operacion, cliente)
        operacionesCerradas.add(operacion)
        operacion.realizarse(self)
    }
    method verificarReserva(operacion, cliente) {
        if (!operacion.puedeRealizarOperacion(cliente)) {
            throw new DomainException (message = "Otra persona ya tiene una reserva sobre esta propiedad")
        }
    }
    method cantidadOperacionesCerradas () = operacionesCerradas.size()
    method cantidadReservas () = reservas.size()
    method totalComisionGanada() = operacionesCerradas.sum({operacion => operacion.comision()})
    
    method vaATenerProblemasCon (otroEmpleado) = self.cerroOperacionEnMismaZonaQue(otroEmpleado) and (self.realizoOperacionDeReservaDe(otroEmpleado) or  otroEmpleado.realizoOperacionDeReservaDe(self))
    
    method cerroOperacionEnMismaZonaQue(otroEmpleado) {
        return operacionesCerradas.any({operacion => otroEmpleado.realizoOperacionEn(operacion.zona())})
    }
    method realizoOperacionEn(zona) = operacionesCerradas.any{operacion => operacion.seRealizoEZona(zona)}

    method realizoOperacionDeReservaDe(otroEmpleado) {
        return operacionesCerradas.any{operacion => otroEmpleado.reservo(operacion)}
    }
    method reservo(operacion) = reservas.contains(operacion)

}