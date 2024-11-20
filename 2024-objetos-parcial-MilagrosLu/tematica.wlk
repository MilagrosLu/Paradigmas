class Tematica {
    const titulo 
    method tituloTematica () = titulo
    method cantidadPalabrasTitulo() = self.tituloTematica().split(" ").size()
    method esInteresante() = false
    method puntosPorOpinarFarandula(unPanelista) = 1
    method puntosExtraoridariosDeportivos() = 1 
    method opinarsePor(unPanelista)  {
        unPanelista.realizarOpinion(self)
    }

}
class Deportiva inherits Tematica {
    override method puntosExtraoridariosDeportivos() = 5
    override method esInteresante() = self.tituloTematica().contains("Messi")
}
class Farandula inherits Tematica {
    const involucradosEnTematica 
    override method puntosPorOpinarFarandula(unPanelista) {
        if (involucradosEnTematica.contains(unPanelista)){
            return self.cantidadDeInvolucrados()
        }
        return 1
    }
    method cantidadDeInvolucrados() = involucradosEnTematica.size()
    override method esInteresante() = involucradosEnTematica >= 3
}
class Filosofica inherits Tematica {
    override method esInteresante() = self.cantidadPalabrasTitulo() > 20
}
class TematicaMixta inherits Tematica{
    const tematicas
    method titulosDeLasTematicas () = tematicas.map{tematica => tematica.tituloTematica()}
    override method tituloTematica() = self.titulosDeLasTematicas().join(" ")
    override method esInteresante() = tematicas.any{tematica => tematica.esInteresante()}
    override method opinarsePor(unPanelista) {
        tematicas.forEach{tematica => tematica.opinarsePor(unPanelista)}
    } 
}

