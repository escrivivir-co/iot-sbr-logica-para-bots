# Integración con ALEPH Scriptorium

> **Submódulo**: `iot-sbr-logica-para-bots`  
> **Rama de integración**: `integration/beta/scriptorium`  
> **Plugin destino**: `prolog-editor`

---

## Arquitectura del Submódulo

```
iot-sbr-logica-para-bots/
├── package.json                    # Orquestador monorepo (concurrently)
├── backend/                        # Express.js + SWI-Prolog
│   ├── src/
│   │   ├── app.js                  # Servidor principal (puerto 8000)
│   │   ├── controllers/
│   │   │   ├── prolog-controller.js    # CRUD reglas + ejecución
│   │   │   └── telemetry-controller.js # IoT telemetría
│   │   ├── services/
│   │   │   ├── prolog-service.js       # 🔑 Motor SWI-Prolog (swipl)
│   │   │   ├── prolog-parser.js        # 🔑 Parser de predicados exportados
│   │   │   ├── template-service.js     # 🔑 Gestión de plantillas
│   │   │   └── codigo/web/plugins/     # 📁 Templates Prolog
│   │   │       ├── state-machine/      # Estado máquina
│   │   │       ├── iot-app/            # IoT básico
│   │   │       └── simu/               # Simulación
│   │   ├── models/
│   │   └── routes/
│   ├── config.js                   # MQTT broker config
│   └── database.sqlite             # SQLite para reglas
│
└── frontend/                       # Angular 14+
    └── src/app/
        ├── components/
        │   ├── rule-editor/        # Editor de reglas Prolog
        │   ├── rule-list/          # Lista de reglas guardadas
        │   ├── dashboard/          # Panel de control
        │   └── telemetry-monitor/  # Monitor IoT
        └── services/
            └── prolog.service.ts   # Cliente HTTP para backend
```

## Tecnologías

| Capa | Tecnología | Versión | Propósito |
|------|------------|---------|-----------|
| Backend | Node.js | 14+ | Runtime |
| Backend | Express.js | 4.17 | API REST |
| Backend | **swipl** | 1.0.6 | Binding SWI-Prolog |
| Backend | SQLite | 5.0.2 | Persistencia de reglas |
| Backend | MQTT | 4.2.8 | Comunicación IoT |
| Frontend | Angular | 14+ | SPA |
| Frontend | TypeScript | 4.x | Tipado |

## Capacidades Prolog

### 1. Motor de Ejecución (`prolog-service.js`)

```javascript
// Inicializa el motor con un template
await prologService.init('state-machine');

// Ejecuta una consulta Prolog
const result = await prologService.executeQuery('do_start(user, Result)');
```

### 2. Parser de Predicados (`prolog-parser.js`)

Extrae automáticamente los predicados exportados de un módulo Prolog:

```prolog
% Entrada: app.pl
:- module(app, [do_init/2, do_start/2, do_pause/2, get_current_state/1]).
```

Genera:
```json
[
  { "predicate": "do_init", "arity": 2, "example": "do_init(Arg1, Arg2)" },
  { "predicate": "do_start", "arity": 2, "example": "do_start(Arg1, Arg2)" },
  { "predicate": "do_pause", "arity": 2, "example": "do_pause(Arg1, Arg2)" },
  { "predicate": "get_current_state", "arity": 1, "example": "get_current_state(Arg1)" }
]
```

### 3. Sistema de Templates

Templates son archivos `.template` (JSON) que definen apps Prolog:

```json
{
  "name": "State Machine",
  "description": "Control basic StateMachine with Prolog engine",
  "main": "state-machine",
  "files": ["."]
}
```

El directorio `plugins/{name}/` contiene los archivos `.pl`.

## Mapeo Ontológico con Scriptorium

| Componente Submódulo | Agente/Plugin Scriptorium | Función |
|---------------------|---------------------------|---------|
| `prolog-service.js` | **PrologEditor** | Motor de ejecución de reglas |
| `template-service.js` | **PrologEditor** | Generación de plantillas |
| `prolog-parser.js` | **PrologEditor ↔ BlocklyEditor** | Extracción de predicados |
| `state-machine.pl` | **ARG_BOARD ↔ AS-GYM** | Máquinas de estado para personajes |
| `rule-editor/` | **PrologEditor ↔ AGENT_CREATOR** | Edición de reglas para agentes |

### Integraciones Clave

#### 1. BlocklyEditor → PrologEditor

```
Blockly (lógica visual) → Exportar → Prolog rules (.pl)
                            ↓
                      PrologEditor (validar, ejecutar)
```

#### 2. AGENT_CREATOR → PrologEditor

```
Receta de agente (JSON) + Reglas Prolog (.pl)
                    ↓
            Agente condicionado por lógica declarativa
```

#### 3. ARG_BOARD → PrologEditor

```
Obra (monomito) → Estadio con condiciones Prolog
                    ↓
            Teatro ejecuta reglas en tiempo real
```

#### 4. AS-GYM FIA → PrologEditor

```
Paradigma SBR (Sistema Basado en Reglas)
        ↓
Prolog como motor de inferencia para agentes
```

## Dependencias Externas

| Dependencia | Tipo | Instalación |
|-------------|------|-------------|
| SWI-Prolog | Sistema | `brew install swi-prolog` (macOS) |
| Node.js | Runtime | `nvm install 18` |
| MQTT Broker | Opcional | `brew install mosquitto` |

## API REST

| Método | Ruta | Descripción |
|--------|------|-------------|
| GET | `/api/sdk-templates` | Lista templates disponibles |
| GET | `/api/template/:name` | Obtiene predicados de un template |
| POST | `/api/rules` | Guarda una regla |
| GET | `/api/rules/:app` | Lista reglas por app |
| POST | `/api/run-rule` | Ejecuta consulta Prolog |
| DELETE | `/api/rules/:id` | Elimina regla |

## Supuestos y Gaps

### Supuestos

1. SWI-Prolog instalado en el sistema del usuario
2. Binding `swipl` compatible con versión de Node
3. Usuario tiene conocimiento de Prolog (perfil académico)

### Gaps Identificados

| Gap | Descripción | Prioridad |
|-----|-------------|-----------|
| G1 | No hay exportación Blockly → Prolog | Alta |
| G2 | Templates no editables desde UI | Media |
| G3 | Sin integración con FIA/red_semantica | Alta |
| G4 | Sin validación sintáctica de Prolog | Media |
| G5 | Sin sistema de almacenamiento en ARCHIVO | Alta |

## Modo de Ejecución

### Desarrollo local

```bash
cd iot-sbr-logica-para-bots
npm install
npm start  # Inicia backend (8000) + frontend (5001)
```

### Verificar SWI-Prolog

```bash
swipl --version
# SWI-Prolog version 9.x.x
```

## Estructura de Plugin Destino

```
.github/plugins/prolog-editor/
├── manifest.md
├── agents/prolog-editor.agent.md
├── prompts/
│   ├── crear-template-prolog.prompt.md
│   ├── ejecutar-consulta.prompt.md
│   ├── exportar-blockly-prolog.prompt.md
│   └── importar-reglas.prompt.md
├── instructions/prolog-editor.instructions.md
└── docs/README.md

ARCHIVO/PLUGINS/PROLOG_EDITOR/
├── templates/           # Plantillas .template + .pl
├── rules/              # Reglas de usuario guardadas
└── exports/            # Exportaciones Blockly → Prolog
```

## Referencias

- [SWI-Prolog](https://www.swi-prolog.org/)
- [swipl npm](https://www.npmjs.com/package/swipl)
- [Plugin BlocklyEditor](/.github/plugins/blockly-editor/)
- [Plugin AS-GYM](/.github/plugins/as-gym/) (pendiente)

