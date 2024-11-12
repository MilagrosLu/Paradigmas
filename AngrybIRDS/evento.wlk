object secionManejoIraConMatilda{
    method realizarse (isla) {
        isla.tranquilizarTodosPajaros()
    }
}
class InvacionCerditos {
    const cantidad
    const veces = []
    method realizarse(isla) {
        self.listaVeces(cantidad)
        veces.forEach({numero => isla.enojarTodosPajaros()})
    }
    method listaVeces(numero) {
        if (numero >= 100) {
            veces.add(1)
            self.listaVeces(numero - 100)
        }
    }
}
class FiestaSorpresa {
    const homenajeados 
    method realizarse(isla) {
       isla.hacerEnojarHomenajeados(homenajeados)
    }

}
class SerieDeEventosDesafortunados {
    const serieEventos
    method realizarse(isla) {
        serieEventos.forEach({evento => evento.realizarse(isla)})
    }
}
