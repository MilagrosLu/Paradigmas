import linea.*
class Pack {
    const vigencia = ilimitado

    method puedeCubirirConsumo (consumo) = vigencia.esValido(consumo.fecha())  && self.puedeSatisfacer(consumo)
    method puedeSatisfacer (consumo)
    method packNoUtil (dia) = !vigencia.esValido(dia) || self.acabado()
    method packDisp (dia) = !self.packNoUtil(dia)
    method acabado() = false
    method consumirse (consumo) {}
}

object ilimitado {
    method esValido(dia) = true 
}
class Vencimeinto {
    const fechaVencimiento 
    method esValido(dia) = dia < fechaVencimiento
}
class CretditoDisponible inherits Pack{
    var creditoDisponible
    override method puedeSatisfacer (consumo) {
        return consumo.costo() <= creditoDisponible
    }
    override method consumirse (consumo) {
        creditoDisponible -= consumo.costo()
    }
    override method acabado() = creditoDisponible <= 0
}
class PackInternet inherits Pack{
    override method puedeSatisfacer (consumo) {
        return consumo.puedeSatisfacerInternet(self)
    }
    method puedeConsumirMegas (megas)
}
class PackLlamada inherits Pack {
    override method puedeSatisfacer (consumo) {
        return consumo.puedeSatisfacerLlamada(self)
    }
    method puedeConsumirLlamadas (segundos)
}
class MegasLibres inherits PackInternet{
    var property megasLibres
    override method puedeConsumirMegas (megas) = megas <= megasLibres
    override method consumirse (consumo) {
        megasLibres -=  consumo.megasConsumidos()
    }
    override method acabado() = megasLibres <= 0
}
class MegasLibresPlusPlus inherits MegasLibres {
    override method puedeConsumirMegas(megas) = super(megas) or megas <= 0.1
}
class InternetIlimitadoFindes inherits PackInternet{
    override method puedeConsumirMegas (megas) = true
}
class LlamadaGratis inherits PackLlamada {
    override method puedeConsumirLlamadas (segundos) = true
}
