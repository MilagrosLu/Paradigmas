import telefonia.*
class Consumo  {
    const property fecha 
    method estaEnRango (inicial, final) = fecha.between(inicial, final)
    method esEnFinde() = fecha.isWeekendDay()
}
class ConsumosInternet  inherits Consumo {
    const property megasConsumidos
    method costo () {
        return megasConsumidos * telefonia.precioMega()
    }
    method puedeSatisfacerInternet (pack) {
        return pack.puedeConsumirMegas(megasConsumidos)
    }
    method puedeSatisfacerLlamada (pack) = false
}
class ConsumosLlamadas inherits Consumo{
    const segundoConsumidos
    method costo () {
        var costoExtra = 0
        if (segundoConsumidos > 30) {
            costoExtra = (segundoConsumidos - 30) * telefonia.precioSegundo()
        }
        return telefonia.precioFijoLlamada() + costoExtra
    }
    method puedeSatisfacerInternet (pack) = false
    method puedeSatisfacerLlamada (pack) = pack.puedeConsumirLlamadas(segundoConsumidos)
}