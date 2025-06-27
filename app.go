package main

import (
	"encoding/json"
	"fmt"
	"net/http"
	"strings"
)

// Tipos de tokens
type TokenType string

const (
	KEYWORD    TokenType = "KEYWORD"
	IDENTIFIER TokenType = "IDENTIFIER"
	OPERATOR   TokenType = "OPERATOR"
	LITERAL    TokenType = "LITERAL"
	DELIMITER  TokenType = "DELIMITER"
	WHITESPACE TokenType = "WHITESPACE"
	NEWLINE    TokenType = "NEWLINE"
	INDENT     TokenType = "INDENT"
	DEDENT     TokenType = "DEDENT"
	UNKNOWN    TokenType = "UNKNOWN"
)

// Token estructura
type Token struct {
	Type     TokenType `json:"type"`
	Value    string    `json:"value"`
	Line     int       `json:"line"`
	Column   int       `json:"column"`
	Position int       `json:"position"`
}

// Palabras reservadas de Python
var pythonKeywords = map[string]bool{
	"def":      true,
	"if":       true,
	"else":     true,
	"elif":     true,
	"return":   true,
	"print":    true,
	"for":      true,
	"while":    true,
	"in":       true,
	"and":      true,
	"or":       true,
	"not":      true,
	"True":     true,
	"False":    true,
	"None":     true,
	"import":   true,
	"from":     true,
	"class":    true,
	"try":      true,
	"except":   true,
	"finally":  true,
	"with":     true,
	"as":       true,
	"pass":     true,
	"break":    true,
	"continue": true,
}

// Operadores de Python
var pythonOperators = map[string]bool{
	"<=": true, ">=": true, "==": true, "!=": true,
	"<": true, ">": true, "=": true,
	"+": true, "-": true, "*": true, "/": true, "//": true, "%": true, "**": true,
	"and": true, "or": true, "not": true,
	"+=": true, "-=": true, "*=": true, "/=": true,
}

// Delimitadores de Python
var pythonDelimiters = map[string]bool{
	"(": true, ")": true, "[": true, "]": true, "{": true, "}": true,
	",": true, ":": true, ";": true, ".": true,
}

// Variable para el análisis semántico
type SemanticAnalyzer struct {
	Variables     map[string]bool
	Functions     map[string]bool
	CurrentScope  string
	Errors        []string
	Warnings      []string
}

// Estructuras para request y response
type RequestBody struct {
	Input string `json:"input"`
}

type AnalysisResult struct {
	TokensLexicos    []Token  `json:"tokensLexicos"`
	EstructuraSintactica string `json:"estructuraSintactica"`
	AnalisisSemantico SemanticResult `json:"analisisSemantico"`
	Errores          []string `json:"errores"`
	Advertencias     []string `json:"advertencias"`
}

type SemanticResult struct {
	VariablesEncontradas []string `json:"variablesEncontradas"`
	FuncionesEncontradas []string `json:"funcionesEncontradas"`
	VerificacionAlcance  []string `json:"verificacionAlcance"`
	ErroresSemanticos   []string `json:"erroresSemanticos"`
}

// Analizador léxico
func tokenize(input string) []Token {
	var tokens []Token
	lines := strings.Split(input, "\n")
	position := 0
	
	for lineNum, line := range lines {
		column := 0
		i := 0
		
		// Manejar indentación
		indentLevel := 0
		for i < len(line) && (line[i] == ' ' || line[i] == '\t') {
			if line[i] == ' ' {
				indentLevel++
			} else {
				indentLevel += 4 // Tab = 4 espacios
			}
			i++
		}
		
		if indentLevel > 0 && strings.TrimSpace(line) != "" {
			tokens = append(tokens, Token{
				Type: INDENT,
				Value: fmt.Sprintf("%d", indentLevel),
				Line: lineNum + 1,
				Column: 1,
				Position: position,
			})
		}
		
		for i < len(line) {
			ch := line[i]
			column = i + 1
			
			// Espacios en blanco
			if ch == ' ' || ch == '\t' {
				i++
				continue
			}
			
			// Cadenas de texto
			if ch == '"' || ch == '\'' {
				quote := ch
				start := i
				i++
				for i < len(line) && line[i] != quote {
					i++
				}
				if i < len(line) {
					i++ // Incluir la comilla de cierre
				}
				tokens = append(tokens, Token{
					Type: LITERAL,
					Value: line[start:i],
					Line: lineNum + 1,
					Column: column,
					Position: position + start,
				})
				continue
			}
			
			// Números
			if ch >= '0' && ch <= '9' {
				start := i
				for i < len(line) && ((line[i] >= '0' && line[i] <= '9') || line[i] == '.') {
					i++
				}
				tokens = append(tokens, Token{
					Type: LITERAL,
					Value: line[start:i],
					Line: lineNum + 1,
					Column: column,
					Position: position + start,
				})
				continue
			}
			
			// Operadores de dos caracteres
			if i+1 < len(line) {
				twoChar := string(line[i:i+2])
				if pythonOperators[twoChar] {
					tokens = append(tokens, Token{
						Type: OPERATOR,
						Value: twoChar,
						Line: lineNum + 1,
						Column: column,
						Position: position + i,
					})
					i += 2
					continue
				}
			}
			
			// Operadores de un caracter
			if pythonOperators[string(ch)] {
				tokens = append(tokens, Token{
					Type: OPERATOR,
					Value: string(ch),
					Line: lineNum + 1,
					Column: column,
					Position: position + i,
				})
				i++
				continue
			}
			
			// Delimitadores
			if pythonDelimiters[string(ch)] {
				tokens = append(tokens, Token{
					Type: DELIMITER,
					Value: string(ch),
					Line: lineNum + 1,
					Column: column,
					Position: position + i,
				})
				i++
				continue
			}
			
			// Identificadores y palabras clave
			if (ch >= 'a' && ch <= 'z') || (ch >= 'A' && ch <= 'Z') || ch == '_' {
				start := i
				for i < len(line) && ((line[i] >= 'a' && line[i] <= 'z') || 
					(line[i] >= 'A' && line[i] <= 'Z') || 
					(line[i] >= '0' && line[i] <= '9') || 
					line[i] == '_') {
					i++
				}
				word := line[start:i]
				
				tokenType := IDENTIFIER
				if pythonKeywords[word] {
					tokenType = KEYWORD
				}
				
				tokens = append(tokens, Token{
					Type: tokenType,
					Value: word,
					Line: lineNum + 1,
					Column: column,
					Position: position + start,
				})
				continue
			}
			
			// Caracteres desconocidos
			tokens = append(tokens, Token{
				Type: UNKNOWN,
				Value: string(ch),
				Line: lineNum + 1,
				Column: column,
				Position: position + i,
			})
			i++
		}
		
		// Agregar token de nueva línea si no es la última línea
		if lineNum < len(lines)-1 {
			tokens = append(tokens, Token{
				Type: NEWLINE,
				Value: "\\n",
				Line: lineNum + 1,
				Column: len(line) + 1,
				Position: position + len(line),
			})
		}
		
		position += len(line) + 1 // +1 para el \n
	}
	
	return tokens
}

// Análisis sintáctico simplificado
func syntacticAnalysis(tokens []Token) string {
	structure := []string{}
	i := 0
	
	for i < len(tokens) {
		token := tokens[i]
		
		switch token.Value {
		case "def":
			if i+1 < len(tokens) && tokens[i+1].Type == IDENTIFIER {
				structure = append(structure, fmt.Sprintf("Definición de función: %s", tokens[i+1].Value))
				i += 2
				// Buscar parámetros
				if i < len(tokens) && tokens[i].Value == "(" {
					params := []string{}
					i++
					for i < len(tokens) && tokens[i].Value != ")" {
						if tokens[i].Type == IDENTIFIER {
							params = append(params, tokens[i].Value)
						}
						i++
					}
					if len(params) > 0 {
						structure = append(structure, fmt.Sprintf("  Parámetros: %s", strings.Join(params, ", ")))
					}
				}
			}
		case "if":
			structure = append(structure, "Estructura condicional IF")
		case "else":
			structure = append(structure, "Estructura ELSE")
		case "return":
			structure = append(structure, "Declaración RETURN")
		case "print":
			structure = append(structure, "Llamada a función PRINT")
		}
		i++
	}
	
	return strings.Join(structure, "\n")
}

// Análisis semántico
func semanticAnalysis(tokens []Token) SemanticResult {
	analyzer := SemanticAnalyzer{
		Variables: make(map[string]bool),
		Functions: make(map[string]bool),
		Errors:    []string{},
		Warnings:  []string{},
	}
	
	var variables []string
	var functions []string
	var scopeChecks []string
	
	i := 0
	for i < len(tokens) {
		token := tokens[i]
		
		switch token.Value {
		case "def":
			// Definición de función
			if i+1 < len(tokens) && tokens[i+1].Type == IDENTIFIER {
				funcName := tokens[i+1].Value
				analyzer.Functions[funcName] = true
				functions = append(functions, funcName)
				scopeChecks = append(scopeChecks, fmt.Sprintf("Función '%s' definida correctamente", funcName))
			}
			
		case "=":
			// Asignación de variable
			if i > 0 && tokens[i-1].Type == IDENTIFIER {
				varName := tokens[i-1].Value
				analyzer.Variables[varName] = true
				variables = append(variables, varName)
				scopeChecks = append(scopeChecks, fmt.Sprintf("Variable '%s' definida", varName))
			}
		}
		
		// Verificar uso de variables y funciones
		if token.Type == IDENTIFIER {
			// Verificar si es una llamada a función
			if i+1 < len(tokens) && tokens[i+1].Value == "(" {
				if !analyzer.Functions[token.Value] && !pythonKeywords[token.Value] {
					// Verificar funciones built-in
					builtins := map[string]bool{"print": true, "len": true, "str": true, "int": true, "float": true}
					if !builtins[token.Value] {
						analyzer.Errors = append(analyzer.Errors, 
							fmt.Sprintf("Función '%s' no está definida (línea %d)", token.Value, token.Line))
					} else {
						scopeChecks = append(scopeChecks, 
							fmt.Sprintf("Función built-in '%s' utilizada correctamente", token.Value))
					}
				}
			} else {
				// Verificar uso de variables
				if !analyzer.Variables[token.Value] && !analyzer.Functions[token.Value] && 
				   !pythonKeywords[token.Value] {
					// Podría ser una variable que se usará después o un parámetro
					analyzer.Warnings = append(analyzer.Warnings, 
						fmt.Sprintf("Variable '%s' podría no estar definida (línea %d)", token.Value, token.Line))
				}
			}
		}
		
		i++
	}
	
	return SemanticResult{
		VariablesEncontradas: variables,
		FuncionesEncontradas: functions,
		VerificacionAlcance:  scopeChecks,
		ErroresSemanticos:   analyzer.Errors,
	}
}

func enableCors(next http.HandlerFunc) http.HandlerFunc {
	return func(w http.ResponseWriter, r *http.Request) {
		w.Header().Set("Access-Control-Allow-Origin", "*")
		w.Header().Set("Access-Control-Allow-Methods", "GET, POST, OPTIONS")
		w.Header().Set("Access-Control-Allow-Headers", "Content-Type")

		if r.Method == http.MethodOptions {
			w.WriteHeader(http.StatusOK)
			return
		}

		next(w, r)
	}
}

func analizarHandler(w http.ResponseWriter, r *http.Request) {
	if r.Method != http.MethodPost {
		http.Error(w, "Solo se permite POST", http.StatusMethodNotAllowed)
		return
	}

	var reqBody RequestBody
	err := json.NewDecoder(r.Body).Decode(&reqBody)
	if err != nil {
		http.Error(w, "JSON inválido", http.StatusBadRequest)
		return
	}

	// Análisis léxico
	tokens := tokenize(reqBody.Input)
	
	// Análisis sintáctico
	syntaxStructure := syntacticAnalysis(tokens)
	
	// Análisis semántico
	semanticResult := semanticAnalysis(tokens)
	
	// Recopilar errores y advertencias
	var allErrors []string
	var allWarnings []string
	
	// Verificar tokens desconocidos
	for _, token := range tokens {
		if token.Type == UNKNOWN {
			allErrors = append(allErrors, fmt.Sprintf("Token desconocido '%s' en línea %d, columna %d", 
				token.Value, token.Line, token.Column))
		}
	}
	
	allErrors = append(allErrors, semanticResult.ErroresSemanticos...)
	
	result := AnalysisResult{
		TokensLexicos:        tokens,
		EstructuraSintactica: syntaxStructure,
		AnalisisSemantico:   semanticResult,
		Errores:             allErrors,
		Advertencias:        allWarnings,
	}

	w.Header().Set("Content-Type", "application/json")
	json.NewEncoder(w).Encode(result)
}

func main() {
	http.HandleFunc("/analizar", enableCors(analizarHandler))
	fmt.Println("Servidor iniciado en puerto 8080")
	fmt.Println("Endpoints disponibles:")
	fmt.Println("  POST /analizar - Analizar código Python")
	http.ListenAndServe(":8080", nil)
}