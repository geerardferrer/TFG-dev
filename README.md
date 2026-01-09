# TFG Learning Dashboard - Ecosistema Completo

Este repositorio contiene la infraestructura completa del ecosistema Learning Dashboard, incluyendo todos los servicios dockerizados y su configuración.

## 📋 Índice

- [Arquitectura del Sistema](#arquitectura-del-sistema)
- [Requisitos Previos](#requisitos-previos)
- [Instalación Rápida](#instalación-rápida)
- [Configuración](#configuración)
- [Servicios Incluidos](#servicios-incluidos)
- [Uso](#uso)
- [Desarrollo](#desarrollo)
- [Troubleshooting](#troubleshooting)

## 🏗️ Arquitectura del Sistema

El ecosistema consta de los siguientes servicios interconectados:

```
┌─────────────────────────────────────────────────────────────┐
│                    Learning Dashboard                        │
│                                                              │
│  ┌──────────────┐  ┌──────────────┐  ┌──────────────┐     │
│  │   Admin Tool │  │  LD Connect  │  │   LD Eval    │     │
│  │   Frontend   │  │   (Webhooks) │  │ (Evaluation) │     │
│  │  (React)     │  │  (Flask)     │  │  (Flask)     │     │
│  └──────┬───────┘  └──────┬───────┘  └──────┬───────┘     │
│         │                 │                  │              │
│         │                 │                  │              │
│  ┌──────▼───────┐  ┌──────▼──────────────────▼───────┐    │
│  │   Admin Tool │  │         Tomcat                   │    │
│  │   Backend    │  │    (LD Dashboard Core)           │    │
│  │ (Spring Boot)│  │                                  │    │
│  └──────┬───────┘  └──────┬───────────────────────────┘   │
│         │                 │                                │
│  ┌──────▼─────────────────▼──────┐  ┌─────────────┐      │
│  │       PostgreSQL               │  │   MongoDB   │      │
│  │  (Proyectos, Estudiantes)      │  │  (Eventos)  │      │
│  └────────────────────────────────┘  └─────────────┘      │
│                                                             │
│  Integraciones Externas:                                   │
│  • GitHub (via webhooks)                                   │
│  • Taiga (via webhooks + ngrok)                           │
└─────────────────────────────────────────────────────────────┘
```

## 📦 Requisitos Previos

- **Docker Desktop** (Windows/Mac) o Docker Engine (Linux)
- **Docker Compose** v2.0 o superior
- **Git** para clonar los submódulos
- **Puertos disponibles**: 5000, 5001, 5432, 5433, 8080, 8888, 27017, 3000

## 🚀 Instalación Rápida

### 1. Clonar el repositorio

```bash
git clone <URL_DE_TU_REPO>
cd TFG-dev
```

### 2. Clonar los submódulos (repositorios internos)

Los repositorios de cada servicio deben estar en sus carpetas correspondientes:

```bash
# Si aún no los tienes clonados:
git clone <URL_LD_LEARNING_DASHBOARD> LD-learning-dashboard
git clone <URL_LD_ADMINTOOL> ld_admintool
git clone <URL_LD_ADMINTOOL_FRONTEND> ld_admintool_frontend
git clone <URL_LD_CONNECT_EVENT> LD_Connect_Event
git clone <URL_LD_EVAL_EVENT> LD_Eval_Event
```

### 3. Configurar variables de entorno

Copia el archivo de ejemplo y edítalo con tus valores:

```bash
cp .env.template .env
```

Edita el archivo `.env` con tus configuraciones (ver [Configuración](#configuración)).

### 4. Iniciar todos los servicios

```bash
docker-compose up -d
```

### 5. Verificar que todo funciona

```bash
docker-compose ps
```

Deberías ver todos los servicios en estado "Up".

## ⚙️ Configuración

### Archivo `.env` principal

El archivo `.env` en la raíz contiene **toda la configuración centralizada**:

```env
# URLs de túneles ngrok (cambiar según sea necesario)
NGROK_LD_URL=https://xxxx.ngrok-free.app
NGROK_LDCONNECT_URL=https://yyyy.ngrok-free.app

# Taiga Configuration
TAIGA_API_URL=https://uncreosoted-dermatic-johnny.ngrok-free.dev/api/v1
TAIGA_AUTH_URL=https://api.taiga.io/api/v1
TAIGA_USERNAME=tu_usuario
TAIGA_PASSWORD=tu_password

# GitHub Configuration
GITHUB_TOKEN=ghp_xxxxxxxxxxxxx

# MongoDB Configuration
MONGO_USER=admin
MONGO_PASSWORD=3LnS985q7tR9

# PostgreSQL Configuration
DB_USER=postgres
DB_PASSWORD=example
```

**Importante**: 
- `TAIGA_API_URL` apunta al ngrok del compañero (para consultas)
- `TAIGA_AUTH_URL` apunta siempre a Taiga público (para autenticación)
- **Nunca** subas el archivo `.env` a Git (está en `.gitignore`)

Ver [CONFIG.md](CONFIG.md) para documentación detallada de todas las variables.

## 🔧 Servicios Incluidos

| Servicio | Puerto | Descripción | Tecnología |
|----------|--------|-------------|------------|
| **Admin Tool Frontend** | 3000 | Interfaz de administración | React + Vite + Nginx |
| **Admin Tool Backend** | 8080 | API de administración | Spring Boot 3.5.6 (Java 17) |
| **LD Connect** | 5000 | Gestión de webhooks GitHub/Taiga | Python 3.9 + Flask |
| **LD Eval** | 5001 | Evaluación y métricas | Python 3.9 + Flask |
| **Tomcat (LD Core)** | 8888 | Learning Dashboard principal | Java + Tomcat 9 |
| **PostgreSQL** | 5433 | Base de datos principal | PostgreSQL 9.6 |
| **MongoDB** | 27017 | Base de datos de eventos | MongoDB 6.0 |

## 📖 Uso

### Acceder a las interfaces

- **Admin Tool**: http://localhost:3000
- **Learning Dashboard**: http://localhost:8888
- **MongoDB Compass**: localhost:27017 (usuario: `admin`, contraseña: `3LnS985q7tR9`)

### Importar equipos desde Excel

1. Accede al Admin Tool en http://localhost:3000
2. Ve a la sección "Importar Equipos"
3. Sube un archivo Excel con el formato especificado
4. El sistema validará los proyectos en Taiga y GitHub
5. Los equipos se crearán automáticamente en el LD

### Webhooks

Los webhooks de GitHub y Taiga envían eventos automáticamente a LD Connect cuando hay cambios:

- **GitHub webhook URL**: `{NGROK_LDCONNECT_URL}/webhook/github?prj=nombre-proyecto`
- **Taiga webhook URL**: `{NGROK_LDCONNECT_URL}/webhook/taiga?prj=nombre-proyecto`

## 🛠️ Desarrollo

### Reconstruir un servicio específico

```bash
# Backend del Admin Tool
docker-compose build admintool_backend
docker-compose up -d admintool_backend

# Frontend del Admin Tool
docker-compose build admintool_frontend
docker-compose up -d admintool_frontend

# LD Connect
docker-compose build ld_connect
docker-compose up -d ld_connect
```

### Ver logs de un servicio

```bash
docker logs -f LDConnect
docker logs -f ld_admintool_backend
docker logs -f admintool_frontend
```

### Reiniciar todo el sistema

Usa el script PowerShell incluido:

```powershell
.\restart-ld.ps1
```

O manualmente:

```bash
docker-compose down
docker-compose up -d
```

### Acceder a la terminal de un contenedor

```bash
docker exec -it LDConnect bash
docker exec -it ld_admintool_backend sh
```

## 🐛 Troubleshooting

### Los contenedores no arrancan

1. Verifica que Docker Desktop esté corriendo
2. Comprueba que los puertos no estén ocupados:
   ```bash
   netstat -ano | findstr :8080
   netstat -ano | findstr :3000
   ```
3. Revisa los logs:
   ```bash
   docker-compose logs
   ```

### Error de autenticación en MongoDB

Si ves errores `Unauthorized` en los logs:

1. Verifica que el `.env` tiene las credenciales correctas
2. Reinicia los servicios:
   ```bash
   docker-compose restart ld_connect ld_eval
   ```

### Error 401 en Taiga API

Si ves `401 Unauthorized` al consultar milestones:

- **Causa**: El ngrok del compañero requiere autenticación
- **Solución**: Verifica que `TAIGA_API_URL` apunta al ngrok correcto y que el servidor ngrok tiene autenticación desactivada

### Webhooks no funcionan

1. Verifica que ngrok esté corriendo y la URL sea accesible
2. Comprueba que el webhook en GitHub/Taiga tenga la URL correcta
3. Revisa los logs de LD Connect:
   ```bash
   docker logs LDConnect --tail 50
   ```

### El Admin Tool no conecta con el backend

1. Verifica que el backend esté corriendo:
   ```bash
   docker ps | grep admintool
   ```
2. Comprueba la configuración de Nginx en el frontend
3. Revisa los logs del backend:
   ```bash
   docker logs ld_admintool_backend
   ```

## 📚 Documentación Adicional

- [CONFIG.md](CONFIG.md) - Documentación completa de configuración
- [TESTING_DOCUMENTATION.md](TESTING_DOCUMENTATION.md) - Documentación de testing
- [LD_Connect_Event/README.md](LD_Connect_Event/README.md) - Documentación de LD Connect
- [LD_Eval_Event/README.md](LD_Eval_Event/README.md) - Documentación de LD Eval

## 🔐 Seguridad

- **Nunca** subas el archivo `.env` a Git
- Usa `.env.template` como referencia para crear tu `.env` local
- Los tokens y contraseñas deben regenerarse en producción
- MongoDB y PostgreSQL solo son accesibles desde `127.0.0.1` (localhost)

## 📝 Notas para la siguiente persona

1. **Ngrok URLs**: Los túneles ngrok cambian cada vez que se reinicia ngrok. Actualiza `NGROK_LD_URL` y `NGROK_LDCONNECT_URL` en el `.env` cuando sea necesario.

2. **Taiga del compañero**: Si el compañero cambia su ngrok, actualiza `TAIGA_API_URL` en el `.env`.

3. **Submódulos Git**: Los repositorios dentro de TFG dev (LD-learning-dashboard, ld_admintool, etc.) son repositorios independientes. Puedes hacer commits y push en cada uno por separado.

4. **Reconstruir contenedores**: Si cambias código Python o Java, necesitas reconstruir el contenedor correspondiente con `docker-compose build <servicio>`.

5. **Base de datos**: Los datos de PostgreSQL y MongoDB se guardan en volúmenes Docker. Persisten aunque reinicies los contenedores.

## 👥 Autores

- Gerard Ferrer - Desarrollo e integración del ecosistema Learning Dashboard

## 📄 Licencia

[Especificar licencia si aplica]
