class Inmueble {
    const zona
    var property puedeVenderse = true
    const property operacionPublicada
    method valor () = zona.plus() + self.valorBase()
    method valorBase()
    
}
class Casa inherits Inmueble {
    const valorParticular
    override method valorBase()  = valorParticular
}
class PH inherits Inmueble {
    const metrosCuadrados
    override method valorBase() = 500000.max(14000 * metrosCuadrados)
}
class Departamento inherits Inmueble {
    const cantidadAmbientes
    override method valorBase()  = 350000 * cantidadAmbientes
}
class Zona {
    var property plus
}
class Local inherits Casa (puedeVenderse = false) {
    var tipo 
    override method valorBase () = tipo.valorTipo(valorParticular)
    method remodelarse (nuevoTipo) {
        tipo = nuevoTipo
    }
    
}
object tipoGalpon {
    method valorTipo(valorParticular) = valorParticular / 2 
}

object tipoLocalALaClase {
    method valorTipo(valorParticular) = valorParticular + valorPlusLocalesALaCalle.plusLocales()
}

object valorPlusLocalesALaCalle {
    const property plusLocales = 4
}