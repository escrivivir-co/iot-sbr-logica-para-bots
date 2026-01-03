import { Component, OnInit, Input, Output, EventEmitter } from '@angular/core';
import { PrologService } from '../../services/prolog.service';

/**
 * Brain Editor Component
 * 
 * Visual editor for .brain.pl files (agent behavior rules).
 * Generates Prolog code from form inputs for dramaturgos.
 * 
 * @epic TEATRO-PROLOG-1.0.0
 */
@Component({
  selector: 'app-brain-editor',
  templateUrl: './brain-editor.component.html',
  styleUrls: ['./brain-editor.component.css']
})
export class BrainEditorComponent implements OnInit {
  @Input() sessionId: string | null = null;
  @Input() agentName: string = '';
  @Input() obraId: string = '';
  @Output() brainExported = new EventEmitter<string>();
  
  // Identity Section
  identity = {
    name: '',
    rol: '',
    especialidad: ''
  };
  
  // Knowledge facts
  facts: string[] = [];
  newFact: string = '';
  
  // Behavior rules
  rules: BehaviorRule[] = [];
  
  // Generated Prolog code
  generatedCode: string = '';
  
  // UI State
  isGenerating: boolean = false;
  isTesting: boolean = false;
  testResult: string | null = null;
  testError: string | null = null;
  
  // Predefined contexts for dropdown
  readonly CONTEXTS = [
    'inicio',
    'buscar_informacion',
    'buscar_ubicacion',
    'buscar_estructura',
    'validar_coherencia',
    'validar_pre_commit',
    'crear_contenido',
    'auditar',
    'desconocido'
  ];
  
  // Predefined actions for dropdown
  readonly ACTIONS = [
    'presentarse',
    'consultar_indice',
    'consultar_funcional',
    'consultar_tecnico',
    'ejecutar_tests',
    'ejecutar_5_tests',
    'invocar_aleph',
    'invocar_banderas',
    'delegar_ox',
    'escalar_a_ox',
    'editar_referencia_dry',
    'sincronizar_codebase'
  ];

  constructor(private prologService: PrologService) {}

  ngOnInit(): void {
    if (this.agentName) {
      this.identity.name = this.agentName;
    }
    this.addDefaultRule();
  }

  /**
   * Add a new empty rule
   */
  addRule(): void {
    this.rules.push({
      context: '',
      action: '',
      customContext: false,
      customAction: false
    });
  }

  /**
   * Add default fallback rule
   */
  private addDefaultRule(): void {
    this.rules.push({
      context: 'desconocido',
      action: 'delegar_ox',
      customContext: false,
      customAction: false
    });
  }

  /**
   * Remove a rule by index
   */
  removeRule(index: number): void {
    if (this.rules.length > 1) {
      this.rules.splice(index, 1);
    }
  }

  /**
   * Add a knowledge fact
   */
  addFact(): void {
    if (this.newFact.trim()) {
      this.facts.push(this.newFact.trim());
      this.newFact = '';
    }
  }

  /**
   * Remove a fact by index
   */
  removeFact(index: number): void {
    this.facts.splice(index, 1);
  }

  /**
   * Generate Prolog code from form data
   */
  generateCode(): void {
    this.isGenerating = true;
    const name = this.identity.name.toLowerCase().replace(/[^a-z0-9_]/g, '_');
    const date = new Date().toISOString().split('T')[0];
    
    let code = `%% ============================================
%% Cerebro Prolog: ${this.identity.name}
%% Obra: ${this.obraId || 'sin_asignar'}
%% Generado por: PrologEditor BrainEditor
%% Fecha: ${date}
%% Versión: 1.0.0
%% ============================================

%% --- MÓDULO ---
:- module(brain_${name}, [
    rol/2,
    especialidad/2,
    decidir_accion/2,
    conoce/2
]).

%% --- IDENTIDAD ---
rol(${name}, ${this.identity.rol || 'sin_rol'}).
especialidad(${name}, ${this.identity.especialidad || 'sin_especialidad'}).

%% --- CONOCIMIENTO BASE ---
conoce(${name}, scriptorium).
conoce(${name}, prolog).
`;

    // Add custom facts
    for (const fact of this.facts) {
      code += `conoce(${name}, ${fact}).\n`;
    }

    code += `
%% --- CONTEXTO ---
:- dynamic contexto/1.
:- dynamic estado_obra/1.

contexto(inicio).
estado_obra(en_cartel).

%% --- REGLAS DE COMPORTAMIENTO ---
decidir_accion(${name}, Accion) :-
    contexto(Contexto),
    regla_para(Contexto, Accion),
    !.

decidir_accion(${name}, delegar_ox) :-
    \\+ contexto(_).

%% --- REGLAS ESPECÍFICAS ---
`;

    // Add user-defined rules
    for (const rule of this.rules) {
      const ctx = rule.context || 'desconocido';
      const act = rule.action || 'delegar_ox';
      code += `regla_para(${ctx}, ${act}).\n`;
    }

    code += `
%% --- TRANSICIONES ---
cambiar_contexto(NuevoContexto) :-
    retractall(contexto(_)),
    assert(contexto(NuevoContexto)).

%% --- HOOKS TEATRO ---
on_estadio_cambio(EstadioId) :-
    format('${name}: Cambio a estadio ~w~n', [EstadioId]).

on_turno(TurnoId) :-
    decidir_accion(${name}, Accion),
    format('${name} turno ~w: ~w~n', [TurnoId, Accion]).

%% --- QUERIES EJEMPLO ---
%% ?- decidir_accion(${name}, X).
%% ?- rol(${name}, R).
%% ?- conoce(${name}, C).

%% --- FIN DEL CEREBRO ---
`;

    this.generatedCode = code;
    this.isGenerating = false;
  }

  /**
   * Test the generated code in a Prolog session
   */
  testCode(): void {
    if (!this.generatedCode) {
      this.testError = 'Generate code first';
      return;
    }
    
    if (!this.sessionId) {
      this.testError = 'No active session. Create a session first.';
      return;
    }

    this.isTesting = true;
    this.testResult = null;
    this.testError = null;
    
    const name = this.identity.name.toLowerCase().replace(/[^a-z0-9_]/g, '_');
    
    // Test query: decidir_accion(name, X)
    this.prologService.runRule(`decidir_accion(${name}, X)`, this.sessionId).subscribe({
      next: (response) => {
        if (response.success && response.payload && response.payload.length > 0) {
          this.testResult = `✅ Test passed! Action: ${JSON.stringify(response.payload)}`;
        } else {
          this.testError = `Query returned false or no results`;
        }
        this.isTesting = false;
      },
      error: (err) => {
        this.testError = 'Test failed: ' + (err.error?.error || err.message || 'Unknown error');
        this.isTesting = false;
      }
    });
  }

  /**
   * Export the generated code
   */
  exportBrain(): void {
    if (!this.generatedCode) {
      this.generateCode();
    }
    
    const name = this.identity.name.toLowerCase().replace(/[^a-z0-9_]/g, '_');
    const filename = `${name}.brain.pl`;
    
    // Create download
    const blob = new Blob([this.generatedCode], { type: 'text/plain' });
    const url = window.URL.createObjectURL(blob);
    const a = document.createElement('a');
    a.href = url;
    a.download = filename;
    a.click();
    window.URL.revokeObjectURL(url);
    
    this.brainExported.emit(filename);
  }

  /**
   * Copy code to clipboard
   */
  copyToClipboard(): void {
    if (this.generatedCode) {
      navigator.clipboard.writeText(this.generatedCode);
    }
  }
}

/**
 * Interface for behavior rules
 */
interface BehaviorRule {
  context: string;
  action: string;
  customContext: boolean;
  customAction: boolean;
}
