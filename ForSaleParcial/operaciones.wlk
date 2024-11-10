import inmobiliaria.*
class Operacion {
    const inmueble
    var property reservada = false
      var reservadaPor 
    method reservase(cliente) {
        reservada = true
        reservadaPor = cliente
    }
    method puedeRealizarOperacion (cliente) = reservadaPor == cliente && self.puedeRealizarse()
    method zona () = inmueble.zona()
    method seRealizoEZona(zona) = inmueble.zona() == zona
    method realizarse(empleado) {
        inmobiliaria.agregarOperacion(self)
        empleado.cobrarComision(self)
    } 
    method puedeRealizarse() = true
}
class Alquiler inherits Operacion {
    const mesesPactados
    
    method comision() = mesesPactados * inmueble.valor() / 50000
}
class Venta inherits Operacion{
    method comision() = inmueble.valor() * inmobiliaria.porcentajeVentaComision()
    override method puedeRealizarse() = inmueble.puedeVenderse()
}
