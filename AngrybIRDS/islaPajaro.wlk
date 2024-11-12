import islaCerdito.*
class IslaPajaro {
    const pajaros 
    method pajarosFuertes () = pajaros.filter {pajaro => pajaro.esPajaroFuerte()}
    method fuezaIsla () = self.pajarosFuertes().sum {pajaro => pajaro.fuerza()}
    method tranquilizarTodosPajaros() {
        pajaros.forEach({pajaro => pajaro.tranquilizarse()})
    }
    method realizarEvanto (evento) {
        evento.realizarse(self)
    }
    method enojarTodosPajaros() {
        self.hacerEnojarPajaros(pajaros)
    }
    method hacerEnojarPajaros(unoPajaros) {
        unoPajaros.forEach({pajaro => pajaro.enojarse()})
    }
    method hacerEnojarHomenajeados (homenajeados){
        const pajarosHomenajeados = self.homenajeadosEnLaIsa(homenajeados)
        self.verificarPajarosHomenajeados(pajarosHomenajeados)
        self.hacerEnojarPajaros(pajarosHomenajeados)
    }   
    method homenajeadosEnLaIsa(homenajeados) = pajaros.filter({pajaro => homenajeados.contains(pajaro)})
    method verificarPajarosHomenajeados(homenajeados){
        if (homenajeados.isEmpty()) {
            throw new DomainException (message = "No se puede realizar fiesta sin homenajeados")
        }
    }

    method atacarIslaCerdito (islaCerdito) {
        pajaros.forEach({pajaro => islaCerdito.recibeAtaque(pajaro)})
    }
    method recuperaronHuevos (islaCerdito) = islaCerdito.sinObtaculos()
}