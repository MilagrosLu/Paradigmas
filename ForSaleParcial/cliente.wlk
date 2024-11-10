class Cliente {

    method solicitarReserva (operacion, empleado) {
        empleado.pedirReserva(operacion, self)
    }
    method solicitarConcretarOperacion (operacion,empleado) {
        empleado.concretarOperacion(operacion, self)
    }
}