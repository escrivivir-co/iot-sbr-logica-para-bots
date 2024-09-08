# Concepto y Exordio

A septiembre 2024, ya ha sido resuesta mi práctica de la asignatura "Fundamentos de Inteligencia Artificial". Un poco como pieza de arte a la vez útil, me resulta sublime la posibilidad acumulada o la potencia latente en una simple semillita. Sostenemos un grano en la palma de la mano sabiendo que, dentro del ecosistema y a través del tiempo generacional, del grano sale un infinito. Y si no un infinito porque los seres vivos están encorsetados en un tiempo finito calculable con polinomios al menos sí será seguro: indeterminista.

Perdón por el "drama" del párrafo anterior, escrupulosamente académicos se anuncia aquí que la PED consistía en usar prolog y participar en un concurso de "ingenios" abierto en categorías. Obteniendo mención honorífica en la que trata el "Desarrollo sostenible", presento aquí una semilla que bien podría engendrar otros proyectos complejos.

La [memoria](./pec2-prolog-memoria.md) y el [código](./codigo) están disponibles en el repositorio.

Sensación agridulce evaluando a posteriori constreñidos a lo que es: "un ejercicio" con sus x horas estimadas para dedicarle. Pero una idea potencialmente extensible.

# Con las manos en la masa

Una vez ha pasado por encima de nuestras cabezas el formato "transformer" y su utilidad en redes pre-entrenadas de tokens, parecería que los sistemas basados en reglas formaran parte de ese conjunto farragoso y de bajo nivel (como lo es el código máquina para los lenguajes de quinta generación) que se solidifica en el armazón pero ya como algo implícito antes que visible a los ingenieros. Al final, la IAs de tipo Situado siguen siendo el paradigma por execelencia para este tipo de sistemas.

Los modelos expertos del conocimiento han demostrado solventemente su distinguido puesto de honor en el tribunal de la lógica que evoluciona desde los casi unarios y atómicos niveles de las proposiciones, alzándose en superconjuntos de predicados, modos, temporalidades, borrosidad imprecisa. Varios ordenes arriba, perdiendo en simplicidad y eficiencia pero ganando en expresividad e interoperatividad, los sistemas de reglas (y prolog con su algoritmo de unificación y resolución a la cabeza), al final, se convierten en los primos listos de la familia lógica. Reúnen en su implementación lo mejor y más fornido del contexto teórico amalgamando unas pocas librerías que permitan acceso al sistema operativo y a la interfaz de usuario. Eso sí, como decimos, siempre restringuidos a las cláusulas de Horno (si esto, entonces) y al proceder ponens/tollens de rigor.

Así pues, en mi opinión los sistemas de reglas han envejecido muy bien ante los enormes radicomas no traceables GPT y, conectándolos al IoT pretendo darles un homenaje. Un sistema de reglas conectado a un sistema situado de sensores/actuadores que proveen entradas y objetivos para poner en funcionamiento la maquinaria del sistema de reglas. Encadenando hacia adelante o hacia atrás, prolog es de backtracking, este ejercicio modeliza un cierto SDK prologo que un ingeniero puede usar para contar con una estructura simple donde la base de conocimientos está preparada con estructuras instanciables (bots) de tipo thing (thing-device-sensor-telemetry) de forma que la base de hechos pueda entrar en un loop de inferencia para la toma de decisiones.

En resumen:
- a) En tiempo de diseño, el ingeniero crea una app sirviéndose del SDK modelizando al menos una o dos Things que definirá vía sus sensores/actuadores.
- b) En tiempo de diseño, el ingeniero crea la red de reglas relacionándolas tanto con el tiempo de vida del sistema como con lo sensores actuadores.
- c) En tiempo de diseño, el automatizador conecta la señales IoT a un parseador de "hechos" prolog.
- d) En tiempo de ejecución, un bucle central crea un pulso de actividad donde las distintas Things toman decisiones de actuación en función de la base de hechos de los sensores.

# Referencias y enlaces

- https://www.uned.es/universidad/inicio/estudios/grados/grado-en-ingenieria-informatica/asignaturas.html?codTitulacion=7101&codAsignatura=71902060&idContenido=1

- https://es.wikipedia.org/wiki/Modus_ponendo_ponens

- https://es.wikipedia.org/wiki/Sistema_experto

- https://es.wikipedia.org/wiki/Internet_de_las_cosas