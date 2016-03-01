package pack;

public abstract class Movil extends ObjetoEspacial {

	private float velocidad;
	private String equipo;

	// Inicializa el móvil con la posición, tamaño, escenario y velocidad
	// recibidos. Lo agrega al escenario.
	public Movil(Point posicion, float tamanio, Escenario escenario, float velocidad) {
		super(posicion, tamanio, escenario);
		this.velocidad = velocidad;
		this.equipo = null;
	}

	public Movil() {
		super();
	}

	public float getVelocidad() {
		return velocidad;
	}

	public void setVelocidad(float velocidad) {
		this.velocidad = velocidad;
	}

	public String getEquipo() {
		return equipo;
	}

	public void setEquipo(String equipo) {
		this.equipo = equipo;
	}

	// Devuelve si el móvil pertenece al mismo equipo del recibido.
	public boolean esAliadoDe(Movil movil) {
		if (this.getEquipo() == null)
			return false;
		return this.getEquipo().equals(movil.getEquipo());
	}

	// Mueve al móvil de acuerdo al tiempo recibido.
	public abstract void moverDurante(float tiempo);

	// Recibe el desplazamiento y mueve al móvil de acuerdo al mismo,
	// respecto a su posición actual.
	public void desplazarEn(Point desplazamiento) {
		this.setPosicion(this.getPosicion().sumarCon(desplazamiento));
	}
	
}

public abstract class ObjetoEspacial {

	private Point posicion;
	private float tamanio;
	private Escenario escenario;
	private boolean destruido;
	private ChoqueDeObjetoEspacial comportamientoAlChocar;
	private String identificacion;
	private Proyeccion proyeccion;

	// Constructor
	// Inicializa el objeto con la posición, tamaño y escenario recibidos.
	// Lo agrega al escenario.
	public ObjetoEspacial(Point posicion, float tamanio, Escenario escenario) {

		this.posicion = posicion;
		this.tamanio = tamanio;
		this.escenario = escenario;
		this.destruido = false;
		this.comportamientoAlChocar = new ChoqueDeObjetoEspacial(this);
		if (this.escenario != null) {
			this.escenario.agregarObjeto(this);
		}
	}

	public ObjetoEspacial() {
		super();
	}

	// Devuelve true si el objeto recibido está superpuesto con el receptor
	// del mensaje. Sino false.
	// Devuelve true si el objeto recibido está superpuesto con el receptor
	// del mensaje. Sino false.
	public boolean estaSuperpuestoCon(ObjetoEspacial objetoEspacial) {

		float distanciaMaxima, distancia;
		distanciaMaxima = this.getTamanio() + objetoEspacial.getTamanio();
		distancia = this.getPosicion().distance(objetoEspacial.getPosicion());
		return distancia < distanciaMaxima;
	}

	// Hace actuar al objeto especial durante el tiempo especificado (si es
	// negativo, se debe ignorar el mensaje). En la implementación, no se
	// debería permitir actuar a objetos que no tengan un escenario, y además,
	// si alguno está destruido, debería desaparecer de su escenario y no
	// realizar nada más.
	public abstract void actuarDurante(float unTiempo);

	public void vivir() {}

	// Hace chocar al objeto recibido con el receptor del mensaje. Ambos sufren
	// el efecto del mismo según cómo esté definido su comportamiento ante el
	// mismo.
	public void chocarCon(ObjetoEspacial unObjetoEspacial) {
		this.responderChoqueDe(unObjetoEspacial);
		unObjetoEspacial.responderChoqueDe(this);
	}

	// Este método tiene carácter de 'protegido de clase' (sólo deben llamarlo
	// objetos de esta dentro de esta jerarquía). El objeto que recibe el
	// mensaje responde el choque y le hace sufrir al objeto recibido sus efectos,
	// de acuerdo al comportamiento frente al choque que este último tenga.
	// El receptor le pide al objeto recibido que le diga cómo tiene que ser
	// afectado al chocar con los demás objetos existentes. Luego es el receptor
	// el que selecciona de entre todo el comportamiento el correspondiente a él
	// mismo.
	public void responderChoqueDe(ObjetoEspacial unObjetoEspacial) {
		unObjetoEspacial.sufrirChoqueDeObjetoEspacial(this);
	}

	// Hace que el objeto receptor del mensaje sea afectado por el bono
	// recibido.
	public void sufrirChoqueDeBono(Bono unBono) {
		this.comportamientoAlChocar.sufrirChoqueDeBono(unBono);
	}

	// Hace que el objeto receptor del mensaje sea afectado por la nave
	// recibida.
	public void sufrirChoqueDeNave(Nave unaNave) {
		this.comportamientoAlChocar.sufrirChoqueDeNave(unaNave);
	}

	// Hace que el objeto receptor del mensaje sea afectado por por la nave
	// civil recibida.
	public void sufrirChoqueDeNaveCivil(NaveCivil unaNaveCivil) {
		this.comportamientoAlChocar.sufrirChoqueDeNaveCivil(unaNaveCivil);
	}

	// Hace que el objeto receptor del mensaje sea afectado por la nave militar
	// recibida.
	public void sufrirChoqueDeNaveMilitar(NaveMilitar unaNaveMilitar) {
		this.comportamientoAlChocar.sufrirChoqueDeNaveMilitar(unaNaveMilitar);
	}

	// Hace que el objeto receptor del mensaje sea afectado por el objeto
	// espacial recibido.
	public void sufrirChoqueDeObjetoEspacial(ObjetoEspacial unObjetoEspacial) {
		this.comportamientoAlChocar
				.sufrirChoqueDeObjetoEspacial(unObjetoEspacial);
	}

	// Hace que el objeto receptor del mensaje sea afectado por el proyectil
	// recibido.
	public void sufrirChoqueDeProyectil(Proyectil unProyectil) {
		this.comportamientoAlChocar.sufrirChoqueDeProyectil(unProyectil);
	}

	// El objeto se borra del escenario. Si no pertenece a ninguno, se levanta
	// una excepción.
	public void desaparecerDelEscenario() {
		this.escenario.borrarObjeto(this);
	}

	// Destruye al objeto y le impide realizar acciones en el futuro.
	public void destruir() {
		this.destruido = true;
	}

	public boolean estaDestruido() {
		return this.destruido;
	}

	public Point getPosicion() {
		return posicion;
	}

	public void setPosicion(Point posicion) {
		this.posicion = posicion;
	}

	public int getX() {
		return (int) this.proyeccion.proyectarPunto(this.posicion).getX();
	}

	public int getY() {
		return (int) this.proyeccion.proyectarPunto(this.posicion).getY();
	}
	
	public void setProyeccion(Proyeccion proyeccion) {
		this.proyeccion = proyeccion;
	}

	public float getTamanio() {
		return this.tamanio;
	}

	public void setTamanio(float nuevoTamanio) {
		this.tamanio = nuevoTamanio;
	}

	public Escenario getEscenario() {
		return this.escenario;
	}

	public String getIdentificacion() {
		return this.identificacion;
	}

	// Asigna una identificación comparable al objeto. Si dos objetos tienen la
	// misma
	// identificación, son del mismo tipo.
	public void setIdentificacion(String nuevaIdentificacion) {
		this.identificacion = nuevaIdentificacion;
	}

	// Cambia el escenario en donde se encuentra el objeto y agrega al mismo
	// en el nuevo.
	public void setEscenario(Escenario nuevoEscenario) {
		this.escenario = nuevoEscenario;
		this.escenario.agregarObjeto(this);
	}

	public ChoqueDeObjetoEspacial getComportamiento() {
		return this.comportamientoAlChocar;
	}

	protected void setComportamiento(
			ChoqueDeObjetoEspacial comportamientoAlChocar) {
		this.comportamientoAlChocar = comportamientoAlChocar;
	}
	
}

public class Point extends Point2D {

	private float x;
	private float y;

	public Point(float x, float y) {
		this.setLocation(x, y);
	}

	public float getX() {
		return this.x;
	}

	public float getY() {
		return this.y;
	}

	public void setLocation(float x, float y) {
		this.x = x;
		this.y = y;
	}

	// Devuelve la suma entre el punto recibido por parámetro y el receptor del
	// mensaje.
	public Point sumarCon(Point punto) {
		return new Point(this.getX() + punto.getX(), this.getY() + punto.getY());
	}

	// Devuelve el punto opuesto al receptor del mensaje.
	public Point getOpuesto() {
		return new Point(-this.getX(), -this.getY());
	}

	// Devuelve el argumento asociado al punto recibido, respecto del origen
	public float argumento() {
		return Math.atan2(this.getY(), this.getX());
	}

	// Devuelve el punto rotado respecto al centro y ángulo (en radianes)
	// recibido.
	public Point rotarEn(float angulo, Point centro) {
		Point relativo = this.sumarCon(centro.getOpuesto());
		float x, y, norma;
		norma = relativo.radio();
		angulo += relativo.argumento();
		x = Math.cos(angulo) * norma;
		y = Math.sin(angulo) * norma;
		Point resultado = centro.sumarCon(new Point(x, y));
		return resultado;
	}

	public Point multiplicar(float multiplicador) {
		return new Point(this.getX() * multiplicador, this.getY()
				* multiplicador);
	}

	public String toString() {
		return String.format("(%.3f, %.3f)", this.getX(), this.getY());
	}

	// Devuelve el radio del punto en el sistema de coordenadas polares
	public float radio() {
		return this.distance(0, 0);
	}

	// Devuelve un punto nuevo normalizado.
	public Point normalizar() {
		float r = this.radio();
		return new Point((this.x / r), (this.y / r));
	}
}

/** Esta clase define el comportamiento receptor de un objeto espacial frente a los 
* demás. Cada método indica cómo se ve afectado el objeto espacial al chocar con
* un tipo particular de objeto.
* Considerando al objeto espacial como el objeto desde el cual todos heredan, 
* los demás comportamientos deberían heredar de éste, entendiendo las siguiente 
* reglas al definir comportamiento:
*	- Una nueva clase de comportamiento indica que el tipo de objeto se ve afectado 
*    de forma diferente a los demás.
*	- Un nuevo método indica que cierto tipo de objeto afecta de diferente manera 
*    a ciertos tipos de objetos.*/
public class ChoqueDeObjetoEspacial{

	private ObjetoEspacial objeto;
	
	// Inicializa el comportamiento del objeto recibido.
	public ChoqueDeObjetoEspacial(ObjetoEspacial unObjetoEspacial) {
		if(unObjetoEspacial == null){
			throw new ComposicionIncompleta("El comportamiento de choque no fue inicializado correctamente.");
		}
		this.objeto = unObjetoEspacial;
	}
	
	public ChoqueDeObjetoEspacial() {}
	
	// Por defecto, a un objeto espacial no le pasa nada cuando choca con un bono.
	public void sufrirChoqueDeBono(Bono unBono) {
		return;
	}
	
	// Por defecto, a un objeto espacial no le pasa nada cuando choca con una nave.
	public void sufrirChoqueDeNave(Nave unaNave) {
		return;
	}
	
	// Por defecto, a un objeto espacial no le pasa nada cuando choca con una nave civil.
	public void sufrirChoqueDeNaveCivil(NaveCivil unaNaveCivil)	{
		return;
	}
	
	// Por defecto, a un objeto espacial no le pasa nada cuando choca con una nave militar.
	public void sufrirChoqueDeNaveMilitar(NaveMilitar unaNaveMilitar) {
		return;
	}
	
	// Por defecto, a un objeto espacial no le pasa nada cuando choca con otro objeto espacial.
	public void sufrirChoqueDeObjetoEspacial(ObjetoEspacial unObjetoEspacial) {
		return;
	}
	
	// Por defecto, a un objeto espacial no le pasa nada cuando choca con un proyectil.
	public void sufrirChoqueDeProyectil(Proyectil unProyectil) {
		return;
	}

	protected ObjetoEspacial getObjeto() {
		return objeto;
	}
	
}

/** El escenario es el lugar en donde viven todos los objetos espaciales que 
* pueden actuar e interactuar entre sí. Un objeto de esta clase puede simular 
* la vida de dichos objetos a lo largo del tiempo, y ellos a la vez pueden 
* comunicarse con él.*/

public class Escenario {

	// Es el área rectangular asignada de combate.
	private Rectangle areaDeCombate;
	// los objetos tienen un id para interactuar entre sí en un único sentido
	// (y que no existan colisiones dobles).
	private int proximoId;
	// Es un diccionario, que tiene como clave a los objetos y como valor el id
	// del mismo.
	// private Map<ObjetoEspacial, Integer> objetos;
	// La puntuación lograda en dicho escenario.
	private int puntuacion;
	private float tiempo;

	/*private Collection<ObjetoEspacial> objetosCreados;
	private Collection<ObjetoEspacial> objetosMuertos;*/

	// Constructor
	// Crea un nuevo escenario cuya área de combate está definida por el
	// rectángulo recibido.
	public Escenario(Rectangle areaDeCombate){
		//objetos = new HashMap<ObjetoEspacial, Integer>();
		this.proximoId = 1;
		this.puntuacion = 0;
		this.areaDeCombate = areaDeCombate;
		this.tiempo = 0;
		/*this.objetosCreados = new HashSet<ObjetoEspacial>();
		this.objetosMuertos = new HashSet<ObjetoEspacial>();*/
	}
	
	public Escenario() {
		super();
	}

	// Agrega un objeto al escenario, el cual ya tiene asignado dicho escenario.
	// El mismo debe estar preparado para actuar cuando sea necesario, de lo
	// contrario se corre el riesgo de que se levante una excepción en el
	// futuro. Si el objeto no tiene asignado este escenario, entonces se
	// levanta una excepción. Si el objeto ya estaba agregado al escenario, no
	// pasa nada.
	public void agregarObjeto(ObjetoEspacial unObjetoEspacial);

	// Borra definitivamente al objeto del escenario. Si el objeto no tiene
	// asignado este escenario, entonces se levanta una excepción.
	public void borrarObjeto(ObjetoEspacial unObjetoEspacial);

	// Devuelve si el objeto recibido se encuentra en el escenario.
	public boolean contieneObjeto(ObjetoEspacial unObjetoEspacial);

	// Devuelve si el objeto se encuentra dentro del área de combate.
	public boolean tieneEnCombateA(ObjetoEspacial unObjetoEspacial) {
		return areaDeCombate.contains(unObjetoEspacial.getPosicion());
	}

	// Hace que avance el tiempo, y le ordena a todos los objetos
	// pertenecientes al escenarios que actúen durante dicho tiempo. En el caso
	// que algún objeto no tenga pueda actuar y tire una excepción, se levantará
	// la misma.
	public void avanzarTiempoEn(float unTiempo);

	// Agrega o quita (si la diferencia es negativa) la cantidad de puntos
	// recibida a la puntuación actual del escenario. La puntuación se mantiene
	// mínimamente en cero.
	public void modificarPuntuacionEn(int unaDiferencia);

	public float getTiempo() {
		return tiempo;
	}

	public int getPuntuacion() {
		return puntuacion;
	}

	/*public Set<ObjetoEspacial> getObjetos() {
		return objetos.keySet();
	}

	// Recibe la referencia a un objeto del escenario. Devuelve una colección
	// de aquellos otros objetos que están colisionando con el mismo y que
	// fueron agregados al escenario después de este (es decir, detecta
	// colisiones en un solo sentido).
	public Set<ObjetoEspacial> getObjetosEnColisionCon(
			ObjetoEspacial unObjetoEspacial) {
		if (unObjetoEspacial.getEscenario() != this) {
			throw new ObjetoDesconocido(
					"El objeto no tiene asignado este escenario.");
		}
		int indiceDelObjeto = objetos.get(unObjetoEspacial);
		Set<ObjetoEspacial> objetosEnColision = new HashSet<ObjetoEspacial>();
		for (ObjetoEspacial objeto : objetos.keySet()) {
			if (objetos.get(objeto) > indiceDelObjeto) {
				if (unObjetoEspacial.estaSuperpuestoCon(objeto)) {
					objetosEnColision.add(objeto);
				}
			}
		}
		return objetosEnColision;
	}

	// Devuelve los objetos agregados al escenario después de la última consulta
	// a este método.
	public Collection<ObjetoEspacial> getObjetosCreados() {
		Collection<ObjetoEspacial> resultado = objetosCreados;
		objetosCreados = new HashSet<ObjetoEspacial>();
		return resultado;
	}

	// Devuelve los objetos borrados del escenario después de la última consulta
	// a este método.
	public Collection<ObjetoEspacial> getObjetosMuertos() {
		Collection<ObjetoEspacial> resultado = objetosMuertos;
		objetosMuertos = new HashSet<ObjetoEspacial>();
		return resultado;
	}*/

	public Rectangle getAreaDeCombate() {
		return areaDeCombate;
	}
	
}

public class Rectangle {
  public boolean contains(Point x);
}

public class Point2D {
  public float distance(float x, float y);
}

public class Proyeccion {	
	public Point proyectarPunto(Point punto);
}

public class Math {
  public float atan2(float x, float y);
  public float sin(float x);
  public float cos(float y);
}

public class String {
  public String format(String f);
}

public class Object {
  public boolean equals(Object object);
}
