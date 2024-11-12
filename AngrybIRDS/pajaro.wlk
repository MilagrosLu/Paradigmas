class Pajaro {
    var ira 
    method enojarse() {
        ira *= 2
    } 
    method fuerza()
    method esPajaroFuerte () = self.fuerza() > 50
    method tranquilizarse() {
        ira =- 5
    }
    method puedeDerribar(obstaculo) = self.fuerza() > obstaculo.resistencia()
    
}
class PajaroComun inherits Pajaro{
    override method fuerza() = ira * 2
}
class Terence inherits Pajaro{
    var cantidadVecesQueSeEnojo = 0 
    const multiplicador
    override method fuerza() = ira * multiplicador * cantidadVecesQueSeEnojo
    override method enojarse() {
        super()
        cantidadVecesQueSeEnojo += 1
    } 
}
class Red inherits Terence (multiplicador = 10) {}
class Chuck inherits Pajaro{
    var velocidadActual
    override method fuerza () {
        var extra = 0
        if (velocidadActual > 80) {
            extra = (velocidadActual - 80) * 5
        }
        return 150 + extra
    }
    override method enojarse() {
        velocidadActual *= 2
    }
    override method tranquilizarse() {
       // no se puede tranquilizar, no hace nada
    }
}
class Bomb inherits PajaroComun {
    const topeMaximo
    override method fuerza () = (super()).min(topeMaximo)
}
class Matilda inherits PajaroComun {
    const huevosPeso
    override method fuerza () = super() *  + self.sumaFuerzaHuevos() 
    method sumaFuerzaHuevos() = huevosPeso.sum()
    override method enojarse() {
        self.ponerHuevo(2)
    }
    method ponerHuevo(peso) {
        huevosPeso.add(peso)
    }
}
