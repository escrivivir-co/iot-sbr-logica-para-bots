# PrologEditor Backend v2.0

REST API Gateway para PrologEditor - conecta el frontend Angular con MCP Prolog Server.

## Arquitectura

```
┌─────────────────┐     HTTP      ┌──────────────────┐      MCP       ┌─────────────────────┐
│  Angular UI     │◄─────────────►│  REST Backend    │◄──────────────►│  MCPPrologServer    │
│  (frontend/)    │               │  (este módulo)   │                │  (mcp-mesh-sdk)     │
└─────────────────┘               └──────────────────┘                └─────────────────────┘
                                         │
                                         ▼
                                   ┌───────────┐
                                   │  SQLite   │
                                   │  (rules)  │
                                   └───────────┘
```

## Cambios en v2.0

### Desacoplamiento de Prolog

- **Antes (v1.x)**: El backend incluía `swipl` directamente
- **Ahora (v2.0)**: El backend actúa como gateway, delegando ejecución a MCPPrologServer

### Beneficios

1. **Separación de responsabilidades**: Backend = API + persistencia, MCP = ejecución Prolog
2. **Escalabilidad**: MCPPrologServer puede ejecutarse en otro proceso/contenedor
3. **Tipos compartidos**: Uso de `@alephscript/mcp-core-sdk` para tipos TypeScript
4. **Sesiones aisladas**: Múltiples sesiones Prolog independientes

## Estructura

```
backend/
├── src/
│   ├── app.ts                 # Entry point
│   ├── controllers/
│   │   ├── prolog.controller.ts    # Handlers HTTP
│   │   └── telemetry.controller.ts
│   ├── routes/
│   │   ├── api.routes.ts      # Endpoints REST
│   │   └── telemetry.routes.ts
│   ├── services/
│   │   ├── mcp-prolog-client.ts    # Cliente MCP
│   │   └── template-service.ts
│   ├── models/
│   │   └── rule.model.ts      # Persistencia SQLite
│   ├── types/
│   │   └── prolog.types.ts    # Tipos compartidos
│   └── utils/
│       └── logger.ts
├── package.json
├── tsconfig.json
└── database.sqlite
```

## API Endpoints

### Legacy (compatible con frontend existente)

| Método | Ruta | Descripción |
|--------|------|-------------|
| GET | `/api/rules` | Listar reglas |
| POST | `/api/rules` | Crear regla |
| DELETE | `/api/rules/:id` | Eliminar regla |
| POST | `/api/run-rule` | Ejecutar query Prolog |
| GET | `/api/sdk-templates` | Listar templates |
| GET | `/api/template/:name` | Cargar template |

### Nuevos (MCP Sessions)

| Método | Ruta | Descripción |
|--------|------|-------------|
| POST | `/api/sessions` | Crear sesión MCP |
| GET | `/api/sessions` | Listar sesiones activas |
| DELETE | `/api/sessions/:id` | Destruir sesión |
| POST | `/api/assert` | Añadir fact a KB |
| POST | `/api/consult` | Cargar archivo .pl |
| GET | `/api/mcp-templates` | Templates del MCP server |

## Instalación

```bash
# Instalar dependencias
npm install

# Build TypeScript
npm run build

# Ejecutar en desarrollo
npm run start:dev

# Ejecutar producción
npm start
```

## Configuración

El cliente MCP se conecta por defecto a:
- **Comando**: `npx ts-node ../../MCPGallery/mcp-mesh-sdk/src/MCPPrologServer.ts`

Para personalizar, modifica `src/services/mcp-prolog-client.ts`.

## Tipos Compartidos

Los tipos se definen en `MCPGallery/mcp-core-sdk/src/types/prolog/index.ts` y se replican localmente en `src/types/prolog.types.ts`.

Cuando `@alephscript/mcp-core-sdk` esté disponible en npm:

```typescript
// En lugar de tipos locales
import type { Rule, QueryResponse } from '@alephscript/mcp-core-sdk/types/prolog';
```

## Especificación OpenAPI

Ver: `ARCHIVO/PLUGINS/OPENASYNCAPI_EDITOR/specs/PrologEditor/openapi.yaml`

## Licencia

AIPL v1.0
