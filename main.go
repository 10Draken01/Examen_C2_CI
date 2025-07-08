package main

import (
	"encoding/json"
	"fmt"
	"log"
	"net/http"
	"strings"
	"unicode"
)

// Estructuras de respuesta
type Token struct {
	Type   string `json:"type"`
	Value  string `json:"value"`
	Line   int    `json:"line"`
	Column int    `json:"column"`
}

type TokenInvalid struct {
	Token
	Reason string `json:"reason"`
}

type Error struct {
	Message string `json:"message"`
	Line    int    `json:"line"`
	Column  int    `json:"column"`
}

type Warning struct {
	Message string `json:"message"`
	Line    int    `json:"line"`
	Column  int    `json:"column"`
}

type Output struct {
	Tokens                 []Token        `json:"tokens"`
	TokensInvalidos        []TokenInvalid `json:"tokens_invalidos"`
	TotalTokens            int            `json:"total_tokens"`
	TotalTokensInvalidos   int            `json:"total_tokens_invalidos"`
	Errores                []Error        `json:"errores"`
	Warnings               []Warning      `json:"warnings"`
}

// Lexer
type Lexer struct {
	input    string
	position int
	line     int
	column   int
}

var javaKeywords = map[string]bool{
	"abstract": true, "boolean": true, "break": true, "byte": true, "case": true,
	"catch": true, "char": true, "class": true, "const": true, "continue": true,
	"default": true, "do": true, "double": true, "else": true, "extends": true,
	"final": true, "finally": true, "float": true, "for": true, "goto": true,
	"if": true, "implements": true, "import": true, "instanceof": true, "int": true,
	"interface": true, "long": true, "native": true, "new": true, "package": true,
	"private": true, "protected": true, "public": true, "return": true, "short": true,
	"static": true, "strictfp": true, "super": true, "switch": true, "synchronized": true,
	"this": true, "throw": true, "throws": true, "transient": true, "try": true,
	"void": true, "volatile": true, "while": true, "String": true,
}

var javaOperators = map[string]bool{
	"+": true, "-": true, "*": true, "/": true, "%": true, "=": true, "==": true,
	"!=": true, "<": true, ">": true, "<=": true, ">=": true, "&&": true, "||": true,
	"!": true, "&": true, "|": true, "^": true, "~": true, "<<": true, ">>": true,
	">>>": true, "++": true, "--": true, "+=": true, "-=": true, "*=": true, "/=": true,
	"%=": true, "&=": true, "|=": true, "^=": true, "<<=": true, ">>=": true, ">>>=": true,
}

var javaSeparators = map[string]bool{
	"(": true, ")": true, "{": true, "}": true, "[": true, "]": true,
	";": true, ",": true, ".": true, ":": true,
}

func NewLexer(input string) *Lexer {
	return &Lexer{
		input:  input,
		line:   1,
		column: 1,
	}
}

func (l *Lexer) peek() rune {
	if l.position >= len(l.input) {
		return 0
	}
	return rune(l.input[l.position])
}

func (l *Lexer) advance() rune {
	if l.position >= len(l.input) {
		return 0
	}
	char := rune(l.input[l.position])
	l.position++
	if char == '\n' {
		l.line++
		l.column = 1
	} else {
		l.column++
	}
	return char
}

func (l *Lexer) skipWhitespace() {
	for unicode.IsSpace(l.peek()) {
		l.advance()
	}
}

func (l *Lexer) skipComment() {
	if l.peek() == '/' {
		next := l.peekNext()
		if next == '/' {
			// Comentario de línea
			for l.peek() != '\n' && l.peek() != 0 {
				l.advance()
			}
		} else if next == '*' {
			// Comentario de bloque
			l.advance() // /
			l.advance() // *
			for {
				if l.peek() == '*' && l.peekNext() == '/' {
					l.advance() // *
					l.advance() // /
					break
				}
				if l.peek() == 0 {
					break
				}
				l.advance()
			}
		}
	}
}

func (l *Lexer) peekNext() rune {
	if l.position+1 >= len(l.input) {
		return 0
	}
	return rune(l.input[l.position+1])
}

func (l *Lexer) readIdentifier() string {
	start := l.position
	for unicode.IsLetter(l.peek()) || unicode.IsDigit(l.peek()) || l.peek() == '_' {
		l.advance()
	}
	return l.input[start:l.position]
}

func (l *Lexer) readNumber() string {
	start := l.position
	for unicode.IsDigit(l.peek()) {
		l.advance()
	}
	if l.peek() == '.' && unicode.IsDigit(l.peekNext()) {
		l.advance() // .
		for unicode.IsDigit(l.peek()) {
			l.advance()
		}
	}
	return l.input[start:l.position]
}

func (l *Lexer) readString() string {
	quote := l.advance() // " o '
	start := l.position - 1
	for l.peek() != quote && l.peek() != 0 {
		if l.peek() == '\\' {
			l.advance() // \
			l.advance() // carácter escapado
		} else {
			l.advance()
		}
	}
	if l.peek() == quote {
		l.advance()
	}
	return l.input[start:l.position]
}

func (l *Lexer) readOperator() string {
	start := l.position
	char := l.advance()
	
	// Operadores de dos caracteres
	if char == '=' && l.peek() == '=' {
		l.advance()
	} else if char == '!' && l.peek() == '=' {
		l.advance()
	} else if char == '<' && (l.peek() == '=' || l.peek() == '<') {
		l.advance()
		if l.input[l.position-1] == '<' && l.peek() == '=' {
			l.advance()
		}
	} else if char == '>' && (l.peek() == '=' || l.peek() == '>') {
		l.advance()
		if l.input[l.position-1] == '>' && l.peek() == '>' {
			l.advance()
			if l.peek() == '=' {
				l.advance()
			}
		} else if l.input[l.position-1] == '>' && l.peek() == '=' {
			l.advance()
		}
	} else if char == '&' && l.peek() == '&' {
		l.advance()
	} else if char == '|' && l.peek() == '|' {
		l.advance()
	} else if char == '+' && (l.peek() == '+' || l.peek() == '=') {
		l.advance()
	} else if char == '-' && (l.peek() == '-' || l.peek() == '=') {
		l.advance()
	} else if char == '*' && l.peek() == '=' {
		l.advance()
	} else if char == '/' && l.peek() == '=' {
		l.advance()
	} else if char == '%' && l.peek() == '=' {
		l.advance()
	}
	
	return l.input[start:l.position]
}

func (l *Lexer) Tokenize() ([]Token, []TokenInvalid) {
	var tokens []Token
	var invalidTokens []TokenInvalid
	maxTokens := 10000 // Límite máximo de tokens para evitar problemas de memoria
	tokenCount := 0
	
	for l.position < len(l.input) && tokenCount < maxTokens {
		l.skipWhitespace()
		
		if l.position >= len(l.input) {
			break
		}
		
		prevPosition := l.position
		line := l.line
		column := l.column
		char := l.peek()
		
		// Comentarios
		if char == '/' && (l.peekNext() == '/' || l.peekNext() == '*') {
			l.skipComment()
			continue
		}
		
		// Strings
		if char == '"' || char == '\'' {
			value := l.readString()
			tokens = append(tokens, Token{
				Type:   "STRING_LITERAL",
				Value:  value,
				Line:   line,
				Column: column,
			})
			tokenCount++
			continue
		}
		
		// Numbers
		if unicode.IsDigit(char) {
			value := l.readNumber()
			tokenType := "INTEGER_LITERAL"
			if strings.Contains(value, ".") {
				tokenType = "FLOAT_LITERAL"
			}
			tokens = append(tokens, Token{
				Type:   tokenType,
				Value:  value,
				Line:   line,
				Column: column,
			})
			tokenCount++
			continue
		}
		
		// Identifiers and keywords
		if unicode.IsLetter(char) || char == '_' {
			value := l.readIdentifier()
			tokenType := "IDENTIFIER"
			if javaKeywords[value] {
				tokenType = "KEYWORD"
			}
			tokens = append(tokens, Token{
				Type:   tokenType,
				Value:  value,
				Line:   line,
				Column: column,
			})
			tokenCount++
			continue
		}
		
		// Separators
		if javaSeparators[string(char)] {
			tokens = append(tokens, Token{
				Type:   "SEPARATOR",
				Value:  string(char),
				Line:   line,
				Column: column,
			})
			l.advance()
			tokenCount++
			continue
		}
		
		// Operators
		operator := l.readOperator()
		if javaOperators[operator] {
			tokens = append(tokens, Token{
				Type:   "OPERATOR",
				Value:  operator,
				Line:   line,
				Column: column,
			})
			tokenCount++
			continue
		}
		
		// Token inválido
		invalidTokens = append(invalidTokens, TokenInvalid{
			Token: Token{
				Type:   "INVALID",
				Value:  string(char),
				Line:   line,
				Column: column,
			},
			Reason: "Carácter no reconocido",
		})
		l.advance()
		tokenCount++
		
		// Protección adicional contra bucles infinitos
		if l.position == prevPosition {
			l.advance() // Forzar avance si no se movió
		}
	}
	
	if tokenCount >= maxTokens {
		invalidTokens = append(invalidTokens, TokenInvalid{
			Token: Token{
				Type:   "ERROR",
				Value:  "MAX_TOKENS_EXCEEDED",
				Line:   l.line,
				Column: l.column,
			},
			Reason: "Se alcanzó el límite máximo de tokens",
		})
	}
	
	return tokens, invalidTokens
}

// Parser y Analizador Semántico
type Node interface {
	String() string
}

type Program struct {
	Classes []ClassDeclaration
}

type ClassDeclaration struct {
	Name       string
	Methods    []MethodDeclaration
	Fields     []FieldDeclaration
	Modifiers  []string
	Line       int
	Column     int
}

type MethodDeclaration struct {
	Name       string
	ReturnType string
	Parameters []Parameter
	Body       []Statement
	Modifiers  []string
	Line       int
	Column     int
}

type FieldDeclaration struct {
	Name      string
	Type      string
	Modifiers []string
	Line      int
	Column    int
}

type Parameter struct {
	Name string
	Type string
}

type Statement interface {
	Node
}

type VariableDeclaration struct {
	Name  string
	Type  string
	Value Expression
	Line  int
	Column int
}

type IfStatement struct {
	Condition Expression
	ThenStmt  Statement
	ElseStmt  Statement
	Line      int
	Column    int
}

type WhileStatement struct {
	Condition Expression
	Body      Statement
	Line      int
	Column    int
}

type ForStatement struct {
	Init      Statement
	Condition Expression
	Update    Statement
	Body      Statement
	Line      int
	Column    int
}

type BlockStatement struct {
	Statements []Statement
	Line       int
	Column     int
}

type ExpressionStatement struct {
	Expression Expression
	Line       int
	Column     int
}

type ReturnStatement struct {
	Value  Expression
	Line   int
	Column int
}

type Expression interface {
	Node
}

type BinaryExpression struct {
	Left     Expression
	Operator string
	Right    Expression
	Line     int
	Column   int
}

type UnaryExpression struct {
	Operator string
	Operand  Expression
	Line     int
	Column   int
}

type MethodCall struct {
	Object    Expression
	Method    string
	Arguments []Expression
	Line      int
	Column    int
}

type FieldAccess struct {
	Object Expression
	Field  string
	Line   int
	Column int
}

type Identifier struct {
	Name   string
	Line   int
	Column int
}

type Literal struct {
	Value  string
	Type   string
	Line   int
	Column int
}

type Assignment struct {
	Left  Expression
	Right Expression
	Line  int
	Column int
}

func (p Program) String() string { return "Program" }
func (c ClassDeclaration) String() string { return "Class: " + c.Name }
func (m MethodDeclaration) String() string { return "Method: " + m.Name }
func (f FieldDeclaration) String() string { return "Field: " + f.Name }
func (v VariableDeclaration) String() string { return "Var: " + v.Name }
func (i IfStatement) String() string { return "If" }
func (w WhileStatement) String() string { return "While" }
func (f ForStatement) String() string { return "For" }
func (b BlockStatement) String() string { return "Block" }
func (e ExpressionStatement) String() string { return "ExprStmt" }
func (r ReturnStatement) String() string { return "Return" }
func (b BinaryExpression) String() string { return "BinaryExpr" }
func (u UnaryExpression) String() string { return "UnaryExpr" }
func (m MethodCall) String() string { return "MethodCall: " + m.Method }
func (f FieldAccess) String() string { return "FieldAccess: " + f.Field }
func (i Identifier) String() string { return "Id: " + i.Name }
func (l Literal) String() string { return "Literal: " + l.Value }
func (a Assignment) String() string { return "Assignment" }

type Parser struct {
	tokens   []Token
	position int
	errors   []Error
	warnings []Warning
}

func NewParser(tokens []Token) *Parser {
	return &Parser{
		tokens: tokens,
	}
}

func (p *Parser) peek() *Token {
	if p.position >= len(p.tokens) {
		return nil
	}
	return &p.tokens[p.position]
}

func (p *Parser) advance() *Token {
	if p.position >= len(p.tokens) {
		return nil
	}
	token := &p.tokens[p.position]
	p.position++
	return token
}

func (p *Parser) expect(tokenType string) *Token {
	token := p.peek()
	if token == nil || token.Type != tokenType {
		if token != nil {
			p.addError(fmt.Sprintf("Se esperaba %s pero se encontró %s", tokenType, token.Type), token.Line, token.Column)
		} else {
			p.addError(fmt.Sprintf("Se esperaba %s pero se alcanzó el final del archivo", tokenType), 0, 0)
		}
		return nil
	}
	return p.advance()
}

func (p *Parser) expectValue(value string) *Token {
	token := p.peek()
	if token == nil || token.Value != value {
		if token != nil {
			p.addError(fmt.Sprintf("Se esperaba '%s' pero se encontró '%s'", value, token.Value), token.Line, token.Column)
		} else {
			p.addError(fmt.Sprintf("Se esperaba '%s' pero se alcanzó el final del archivo", value), 0, 0)
		}
		return nil
	}
	return p.advance()
}

func (p *Parser) addError(message string, line, column int) {
	p.errors = append(p.errors, Error{
		Message: message,
		Line:    line,
		Column:  column,
	})
}

func (p *Parser) addWarning(message string, line, column int) {
	p.warnings = append(p.warnings, Warning{
		Message: message,
		Line:    line,
		Column:  column,
	})
}

func (p *Parser) Parse() (*Program, []Error, []Warning) {
	program := &Program{}
	maxClasses := 100 // Límite de clases
	classCount := 0
	
	for p.peek() != nil && classCount < maxClasses {
		prevPosition := p.position
		
		if class := p.parseClass(); class != nil {
			program.Classes = append(program.Classes, *class)
		} else {
			// Si no puede parsear una clase y no avanza, forzar avance para evitar bucle infinito
			if p.position == prevPosition {
				p.advance()
			}
		}
		
		classCount++
		
		if classCount >= maxClasses {
			p.addError("Demasiadas clases, posible bucle infinito detectado", 0, 0)
			break
		}
	}
	
	return program, p.errors, p.warnings
}

func (p *Parser) parseClass() *ClassDeclaration {
	var modifiers []string
	
	// Parse modifiers
	for p.peek() != nil && (p.peek().Value == "public" || p.peek().Value == "private" || p.peek().Value == "protected" || p.peek().Value == "static" || p.peek().Value == "final" || p.peek().Value == "abstract") {
		modifiers = append(modifiers, p.advance().Value)
	}
	
	if p.expectValue("class") == nil {
		return nil
	}
	
	nameToken := p.expect("IDENTIFIER")
	if nameToken == nil {
		return nil
	}
	
	class := &ClassDeclaration{
		Name:      nameToken.Value,
		Modifiers: modifiers,
		Line:      nameToken.Line,
		Column:    nameToken.Column,
	}
	
	if p.expectValue("{") == nil {
		return nil
	}
	
	// Parse class body con protección contra bucles infinitos
	maxMembers := 500 // Límite de miembros por clase
	memberCount := 0
	
	for p.peek() != nil && p.peek().Value != "}" && memberCount < maxMembers {
		prevPosition := p.position
		
		if method := p.parseMethod(); method != nil {
			class.Methods = append(class.Methods, *method)
		} else if field := p.parseField(); field != nil {
			class.Fields = append(class.Fields, *field)
		} else {
			// Si no se pudo parsear y no avanzamos, forzar avance
			if p.position == prevPosition {
				p.advance()
			}
		}
		
		memberCount++
		
		if memberCount >= maxMembers {
			p.addError("Demasiados miembros en la clase, posible bucle infinito detectado", 0, 0)
			break
		}
	}
	
	p.expectValue("}")
	return class
}

func (p *Parser) parseMethod() *MethodDeclaration {
	start := p.position
	var modifiers []string
	
	// Parse modifiers
	for p.peek() != nil && (p.peek().Value == "public" || p.peek().Value == "private" || p.peek().Value == "protected" || p.peek().Value == "static" || p.peek().Value == "final") {
		modifiers = append(modifiers, p.advance().Value)
	}
	
	// Parse return type
	returnTypeToken := p.peek()
	if returnTypeToken == nil || (returnTypeToken.Type != "KEYWORD" && returnTypeToken.Type != "IDENTIFIER") {
		p.position = start
		return nil
	}
	returnType := p.advance().Value
	
	// Parse method name
	nameToken := p.peek()
	if nameToken == nil || nameToken.Type != "IDENTIFIER" {
		p.position = start
		return nil
	}
	name := p.advance().Value
	
	// Check for opening parenthesis
	if p.peek() == nil || p.peek().Value != "(" {
		p.position = start
		return nil
	}
	p.advance() // consume (
	
	method := &MethodDeclaration{
		Name:       name,
		ReturnType: returnType,
		Modifiers:  modifiers,
		Line:       nameToken.Line,
		Column:     nameToken.Column,
	}
	
	// Parse parameters
	for p.peek() != nil && p.peek().Value != ")" {
		param := p.parseParameter()
		if param != nil {
			method.Parameters = append(method.Parameters, *param)
		}
		
		if p.peek() != nil && p.peek().Value == "," {
			p.advance()
		}
	}
	
	if p.expectValue(")") == nil {
		return nil
	}
	
	// Parse method body
	if p.peek() != nil && p.peek().Value == "{" {
		body := p.parseBlock()
		if body != nil {
			method.Body = body.Statements
		}
	}
	
	return method
}

func (p *Parser) parseField() *FieldDeclaration {
	start := p.position
	var modifiers []string
	
	// Parse modifiers
	for p.peek() != nil && (p.peek().Value == "public" || p.peek().Value == "private" || p.peek().Value == "protected" || p.peek().Value == "static" || p.peek().Value == "final") {
		modifiers = append(modifiers, p.advance().Value)
	}
	
	// Parse type
	typeToken := p.peek()
	if typeToken == nil || (typeToken.Type != "KEYWORD" && typeToken.Type != "IDENTIFIER") {
		p.position = start
		return nil
	}
	fieldType := p.advance().Value
	
	// Parse field name
	nameToken := p.peek()
	if nameToken == nil || nameToken.Type != "IDENTIFIER" {
		p.position = start
		return nil
	}
	name := p.advance().Value
	
	// Check for semicolon (to distinguish from method)
	if p.peek() == nil || p.peek().Value != ";" {
		p.position = start
		return nil
	}
	p.advance() // consume ;
	
	return &FieldDeclaration{
		Name:      name,
		Type:      fieldType,
		Modifiers: modifiers,
		Line:      nameToken.Line,
		Column:    nameToken.Column,
	}
}

func (p *Parser) parseParameter() *Parameter {
	typeToken := p.expect("IDENTIFIER")
	if typeToken == nil {
		return nil
	}
	
	nameToken := p.expect("IDENTIFIER")
	if nameToken == nil {
		return nil
	}
	
	return &Parameter{
		Type: typeToken.Value,
		Name: nameToken.Value,
	}
}

func (p *Parser) parseBlock() *BlockStatement {
	if p.expectValue("{") == nil {
		return nil
	}
	
	block := &BlockStatement{}
	maxStatements := 1000 // Límite para evitar bucles infinitos
	statementCount := 0
	
	for p.peek() != nil && p.peek().Value != "}" && statementCount < maxStatements {
		prevPosition := p.position
		stmt := p.parseStatement()
		
		if stmt != nil {
			block.Statements = append(block.Statements, stmt)
		} else {
			// Si no se pudo parsear y no avanzamos, forzar avance para evitar bucle infinito
			if p.position == prevPosition {
				p.advance()
			}
		}
		
		statementCount++
		
		// Verificación adicional de seguridad
		if statementCount >= maxStatements {
			p.addError("Demasiadas declaraciones en el bloque, posible bucle infinito detectado", 0, 0)
			break
		}
	}
	
	p.expectValue("}")
	return block
}

func (p *Parser) parseStatement() Statement {
	token := p.peek()
	if token == nil {
		return nil
	}
	
	switch token.Value {
	case "if":
		return p.parseIfStatement()
	case "while":
		return p.parseWhileStatement()
	case "for":
		return p.parseForStatement()
	case "return":
		return p.parseReturnStatement()
	case "{":
		return p.parseBlock()
	default:
		// Try to parse as variable declaration or expression
		
if p.isVariableDeclaration() {
	stmts := p.parseVariableDeclarations()
	if len(stmts) > 0 {
		if len(stmts) == 1 {
			return stmts[0]
		}
		return &BlockStatement{
			Statements: stmts,
			Line: stmts[0].(*VariableDeclaration).Line,
			Column: stmts[0].(*VariableDeclaration).Column,
		}
	}
}

		return p.parseExpressionStatement()
	}
}

func (p *Parser) isVariableDeclaration() bool {
	// Simple heuristic: type followed by identifier
	if p.position+1 < len(p.tokens) {
		current := p.tokens[p.position]
		next := p.tokens[p.position+1]
		return (current.Type == "KEYWORD" || current.Type == "IDENTIFIER") && next.Type == "IDENTIFIER"
	}
	return false
}


func (p *Parser) parseVariableDeclarations() []Statement {
	var declarations []Statement

	typeToken := p.advance()
	firstName := p.expect("IDENTIFIER")
	if firstName == nil {
		return nil
	}

	var value Expression
	if p.peek() != nil && p.peek().Value == "=" {
		p.advance()
		value = p.parseExpression()
	}

	declarations = append(declarations, &VariableDeclaration{
		Type:   typeToken.Value,
		Name:   firstName.Value,
		Value:  value,
		Line:   firstName.Line,
		Column: firstName.Column,
	})

	for p.peek() != nil && p.peek().Value == "," {
		p.advance()
		nameToken := p.expect("IDENTIFIER")
		if nameToken == nil {
			break
		}
		var val Expression
		if p.peek() != nil && p.peek().Value == "=" {
			p.advance()
			val = p.parseExpression()
		}
		declarations = append(declarations, &VariableDeclaration{
			Type:   typeToken.Value,
			Name:   nameToken.Value,
			Value:  val,
			Line:   nameToken.Line,
			Column: nameToken.Column,
		})
	}

	p.expectValue(";")
	return declarations
}


func (p *Parser) parseIfStatement() *IfStatement {
	ifToken := p.expectValue("if")
	if ifToken == nil {
		return nil
	}
	
	if p.expectValue("(") == nil {
		return nil
	}
	condition := p.parseExpression()
	if p.expectValue(")") == nil {
		return nil
	}
	
	thenStmt := p.parseStatement()
	
	var elseStmt Statement
	if p.peek() != nil && p.peek().Value == "else" {
		p.advance()
		elseStmt = p.parseStatement()
	}
	
	return &IfStatement{
		Condition: condition,
		ThenStmt:  thenStmt,
		ElseStmt:  elseStmt,
		Line:      ifToken.Line,
		Column:    ifToken.Column,
	}
}

func (p *Parser) parseWhileStatement() *WhileStatement {
	whileToken := p.expectValue("while")
	if whileToken == nil {
		return nil
	}
	
	if p.expectValue("(") == nil {
		return nil
	}
	condition := p.parseExpression()
	if p.expectValue(")") == nil {
		return nil
	}
	
	body := p.parseStatement()
	
	return &WhileStatement{
		Condition: condition,
		Body:      body,
		Line:      whileToken.Line,
		Column:    whileToken.Column,
	}
}

func (p *Parser) parseForStatement() *ForStatement {
	forToken := p.expectValue("for")
	if forToken == nil {
		return nil
	}
	
	if p.expectValue("(") == nil {
		return nil
	}
	init := p.parseStatement()
	condition := p.parseExpression()
	if p.expectValue(";") == nil {
		return nil
	}
	update := p.parseStatement()
	if p.expectValue(")") == nil {
		return nil
	}
	
	body := p.parseStatement()
	
	return &ForStatement{
		Init:      init,
		Condition: condition,
		Update:    update,
		Body:      body,
		Line:      forToken.Line,
		Column:    forToken.Column,
	}
}

func (p *Parser) parseReturnStatement() *ReturnStatement {
	returnToken := p.expectValue("return")
	if returnToken == nil {
		return nil
	}
	
	var value Expression
	if p.peek() != nil && p.peek().Value != ";" {
		value = p.parseExpression()
	}
	
	p.expectValue(";")
	
	return &ReturnStatement{
		Value:  value,
		Line:   returnToken.Line,
		Column: returnToken.Column,
	}
}

func (p *Parser) parseExpressionStatement() *ExpressionStatement {
	expr := p.parseExpression()
	if expr == nil {
		return nil
	}
	
	p.expectValue(";")
	
	return &ExpressionStatement{
		Expression: expr,
	}
}

func (p *Parser) parseExpression() Expression {
	return p.parseAssignment()
}

func (p *Parser) parseAssignment() Expression {
	expr := p.parseLogicalOr()
	
	if p.peek() != nil && p.peek().Value == "=" {
		p.advance()
		right := p.parseAssignment()
		return &Assignment{
			Left:  expr,
			Right: right,
		}
	}
	
	return expr
}

func (p *Parser) parseLogicalOr() Expression {
	expr := p.parseLogicalAnd()
	
	for p.peek() != nil && p.peek().Value == "||" {
		op := p.advance()
		right := p.parseLogicalAnd()
		expr = &BinaryExpression{
			Left:     expr,
			Operator: op.Value,
			Right:    right,
			Line:     op.Line,
			Column:   op.Column,
		}
	}
	
	return expr
}

func (p *Parser) parseLogicalAnd() Expression {
	expr := p.parseEquality()
	
	for p.peek() != nil && p.peek().Value == "&&" {
		op := p.advance()
		right := p.parseEquality()
		expr = &BinaryExpression{
			Left:     expr,
			Operator: op.Value,
			Right:    right,
			Line:     op.Line,
			Column:   op.Column,
		}
	}
	
	return expr
}

func (p *Parser) parseEquality() Expression {
	expr := p.parseRelational()
	
	for p.peek() != nil && (p.peek().Value == "==" || p.peek().Value == "!=") {
		op := p.advance()
		right := p.parseRelational()
		expr = &BinaryExpression{
			Left:     expr,
			Operator: op.Value,
			Right:    right,
			Line:     op.Line,
			Column:   op.Column,
		}
	}
	
	return expr
}

func (p *Parser) parseRelational() Expression {
	expr := p.parseAdditive()
	
	for p.peek() != nil && (p.peek().Value == "<" || p.peek().Value == ">" || p.peek().Value == "<=" || p.peek().Value == ">=") {
		op := p.advance()
		right := p.parseAdditive()
		expr = &BinaryExpression{
			Left:     expr,
			Operator: op.Value,
			Right:    right,
			Line:     op.Line,
			Column:   op.Column,
		}
	}
	
	return expr
}

func (p *Parser) parseAdditive() Expression {
	expr := p.parseMultiplicative()
	
	for p.peek() != nil && (p.peek().Value == "+" || p.peek().Value == "-") {
		op := p.advance()
		right := p.parseMultiplicative()
		expr = &BinaryExpression{
			Left:     expr,
			Operator: op.Value,
			Right:    right,
			Line:     op.Line,
			Column:   op.Column,
		}
	}
	
	return expr
}

func (p *Parser) parseMultiplicative() Expression {
	expr := p.parseUnary()
	
	for p.peek() != nil && (p.peek().Value == "*" || p.peek().Value == "/" || p.peek().Value == "%") {
		op := p.advance()
		right := p.parseUnary()
		expr = &BinaryExpression{
			Left:     expr,
			Operator: op.Value,
			Right:    right,
			Line:     op.Line,
			Column:   op.Column,
		}
	}
	
	return expr
}

func (p *Parser) parseUnary() Expression {
	if p.peek() != nil && (p.peek().Value == "!" || p.peek().Value == "-" || p.peek().Value == "+") {
		op := p.advance()
		operand := p.parseUnary()
		return &UnaryExpression{
			Operator: op.Value,
			Operand:  operand,
			Line:     op.Line,
			Column:   op.Column,
		}
	}
	
	return p.parsePostfix()
}

func (p *Parser) parsePostfix() Expression {
	expr := p.parsePrimary()
	maxChain := 50 // Límite para evitar cadenas infinitas de acceso
	chainCount := 0
	
	for chainCount < maxChain {
		prevPosition := p.position
		
		if p.peek() != nil && p.peek().Value == "." {
			p.advance() // consume .
			fieldToken := p.expect("IDENTIFIER")
			if fieldToken == nil {
				break
			}
			
			// Check if it's a method call
			if p.peek() != nil && p.peek().Value == "(" {
				p.advance() // consume (
				var args []Expression
				argCount := 0
				maxArgs := 20 // Límite de argumentos
				
				for p.peek() != nil && p.peek().Value != ")" && argCount < maxArgs {
					arg := p.parseExpression()
					if arg != nil {
						args = append(args, arg)
					}
					
					if p.peek() != nil && p.peek().Value == "," {
						p.advance()
					} else {
						break
					}
					argCount++
				}
				
				p.expectValue(")")
				
				expr = &MethodCall{
					Object:    expr,
					Method:    fieldToken.Value,
					Arguments: args,
					Line:      fieldToken.Line,
					Column:    fieldToken.Column,
				}
			} else {
				expr = &FieldAccess{
					Object: expr,
					Field:  fieldToken.Value,
					Line:   fieldToken.Line,
					Column: fieldToken.Column,
				}
			}
			
			// Verificar que avanzamos en el parsing
			if p.position == prevPosition {
				break
			}
		} else {
			break
		}
		
		chainCount++
	}
	
	return expr
}

func (p *Parser) parsePrimary() Expression {
	token := p.peek()
	if token == nil {
		return nil
	}
	
	switch token.Type {
	case "IDENTIFIER":
		p.advance()
		return &Identifier{
			Name:   token.Value,
			Line:   token.Line,
			Column: token.Column,
		}
	case "INTEGER_LITERAL", "FLOAT_LITERAL", "STRING_LITERAL":
		p.advance()
		return &Literal{
			Value:  token.Value,
			Type:   token.Type,
			Line:   token.Line,
			Column: token.Column,
		}
	case "SEPARATOR":
		if token.Value == "(" {
			p.advance() // consume (
			expr := p.parseExpression()
			p.expectValue(")")
			return expr
		}
	}
	
	return nil
}

// Analizador Semántico
type SemanticAnalyzer struct {
	scopes       []map[string]VariableInfo
	currentClass *ClassDeclaration
	errors       []Error
	warnings     []Warning
}

type VariableInfo struct {
	Type   string
	Line   int
	Column int
}

func NewSemanticAnalyzer() *SemanticAnalyzer {
	analyzer := &SemanticAnalyzer{
		scopes: []map[string]VariableInfo{make(map[string]VariableInfo)},
	}
	// Asegurar que no sea nil
	if analyzer.scopes == nil {
		analyzer.scopes = []map[string]VariableInfo{make(map[string]VariableInfo)}
	}
	return analyzer
}

func (s *SemanticAnalyzer) addError(message string, line, column int) {
	s.errors = append(s.errors, Error{
		Message: message,
		Line:    line,
		Column:  column,
	})
}

func (s *SemanticAnalyzer) addWarning(message string, line, column int) {
	s.warnings = append(s.warnings, Warning{
		Message: message,
		Line:    line,
		Column:  column,
	})
}

func (s *SemanticAnalyzer) pushScope() {
	s.scopes = append(s.scopes, make(map[string]VariableInfo))
}

func (s *SemanticAnalyzer) popScope() {
	if len(s.scopes) > 1 {
		s.scopes = s.scopes[:len(s.scopes)-1]
	}
}

func (s *SemanticAnalyzer) declareVariable(name, varType string, line, column int) {
	scope := s.scopes[len(s.scopes)-1]
	if _, exists := scope[name]; exists {
		s.addError(fmt.Sprintf("Variable '%s' ya está declarada en este ámbito", name), line, column)
	}
	scope[name] = VariableInfo{Type: varType, Line: line, Column: column}
}

func (s *SemanticAnalyzer) lookupVariable(name string) (VariableInfo, bool) {
	for i := len(s.scopes) - 1; i >= 0; i-- {
		if info, exists := s.scopes[i][name]; exists {
			return info, true
		}
	}
	return VariableInfo{}, false
}

func (s *SemanticAnalyzer) Analyze(program *Program) ([]Error, []Warning) {
	if program == nil {
		s.addError("Programa es nil", 0, 0)
		return s.errors, s.warnings
	}
	
	s.errors = []Error{}
	s.warnings = []Warning{}
	
	for _, class := range program.Classes {
		s.analyzeClass(&class)
	}
	
	return s.errors, s.warnings
}

func (s *SemanticAnalyzer) analyzeClass(class *ClassDeclaration) {
	if class == nil {
		return
	}
	
	s.currentClass = class
	
	// Check class name conventions
	if !isCapitalized(class.Name) {
		s.addWarning(fmt.Sprintf("El nombre de la clase '%s' debería comenzar con mayúscula", class.Name), class.Line, class.Column)
	}
	
	// Analyze fields
	for _, field := range class.Fields {
		s.analyzeField(&field)
	}
	
	// Analyze methods
	for _, method := range class.Methods {
		s.analyzeMethod(&method)
	}
}

func (s *SemanticAnalyzer) analyzeField(field *FieldDeclaration) {
	if !isValidType(field.Type) {
		s.addError(fmt.Sprintf("Tipo '%s' no reconocido", field.Type), field.Line, field.Column)
	}
	
	// Check field name conventions
	if !isLowerCamelCase(field.Name) {
		s.addWarning(fmt.Sprintf("El nombre del campo '%s' debería estar en camelCase", field.Name), field.Line, field.Column)
	}
}

func (s *SemanticAnalyzer) analyzeMethod(method *MethodDeclaration) {
	if method == nil {
		return
	}
	
	s.pushScope()
	defer s.popScope()
	
	// Check return type
	if !isValidType(method.ReturnType) {
		s.addError(fmt.Sprintf("Tipo de retorno '%s' no reconocido", method.ReturnType), method.Line, method.Column)
	}
	
	// Check method name conventions
	if !isLowerCamelCase(method.Name) {
		s.addWarning(fmt.Sprintf("El nombre del método '%s' debería estar en camelCase", method.Name), method.Line, method.Column)
	}
	
	// Declare parameters
	for _, param := range method.Parameters {
		if !isValidType(param.Type) {
			s.addError(fmt.Sprintf("Tipo de parámetro '%s' no reconocido", param.Type), method.Line, method.Column)
		}
		s.declareVariable(param.Name, param.Type, method.Line, method.Column)
	}
	
	// Analyze method body
	for _, stmt := range method.Body {
		if stmt != nil {
			s.analyzeStatement(stmt)
		}
	}
}

func (s *SemanticAnalyzer) analyzeStatement(stmt Statement) {
	// Verificar si el statement es nil
	if stmt == nil {
		return
	}
	
	// Protección contra recursión excesiva
	if len(s.scopes) > 100 {
		s.addError("Anidamiento demasiado profundo detectado", 0, 0)
		return
	}
	
	switch st := stmt.(type) {
	case *VariableDeclaration:
		if st == nil {
			return
		}
		if !isValidType(st.Type) {
			s.addError(fmt.Sprintf("Tipo '%s' no reconocido", st.Type), st.Line, st.Column)
		}
		s.declareVariable(st.Name, st.Type, st.Line, st.Column)
		if st.Value != nil {
			valueType := s.analyzeExpression(st.Value)
			if valueType != "" && !s.isAssignableType(st.Type, valueType) {
				s.addError(fmt.Sprintf("No se puede asignar %s a variable de tipo %s", valueType, st.Type), st.Line, st.Column)
			}
		}
	case *IfStatement:
		if st == nil {
			return
		}
		if st.Condition != nil {
			condType := s.analyzeExpression(st.Condition)
			if condType != "" && condType != "boolean" {
				s.addError("La condición del if debe ser de tipo boolean", st.Line, st.Column)
			}
		}
		if st.ThenStmt != nil {
			s.analyzeStatement(st.ThenStmt)
		}
		if st.ElseStmt != nil {
			s.analyzeStatement(st.ElseStmt)
		}
	case *WhileStatement:
		if st == nil {
			return
		}
		if st.Condition != nil {
			condType := s.analyzeExpression(st.Condition)
			if condType != "" && condType != "boolean" {
				s.addError("La condición del while debe ser de tipo boolean", st.Line, st.Column)
			}
		}
		if st.Body != nil {
			s.analyzeStatement(st.Body)
		}
	case *ForStatement:
		if st == nil {
			return
		}
		s.pushScope()
		defer s.popScope()
		if st.Init != nil {
			s.analyzeStatement(st.Init)
		}
		if st.Condition != nil {
			condType := s.analyzeExpression(st.Condition)
			if condType != "" && condType != "boolean" {
				s.addError("La condición del for debe ser de tipo boolean", st.Line, st.Column)
			}
		}
		if st.Update != nil {
			s.analyzeStatement(st.Update)
		}
		if st.Body != nil {
			s.analyzeStatement(st.Body)
		}
	case *BlockStatement:
		if st == nil {
			return
		}
		s.pushScope()
		defer s.popScope()
		
		// Limitar número de declaraciones por bloque
		maxStatements := 1000
		for i, blockStmt := range st.Statements {
			if i >= maxStatements {
				s.addError("Demasiadas declaraciones en el bloque", 0, 0)
				break
			}
			if blockStmt != nil {
				s.analyzeStatement(blockStmt)
			}
		}
	case *ExpressionStatement:
		if st == nil {
			return
		}
		if st.Expression != nil {
			s.analyzeExpression(st.Expression)
		}
	case *ReturnStatement:
		if st == nil {
			return
		}
		if st.Value != nil {
			s.analyzeExpression(st.Value)
		}
	default:
		// Statement desconocido o nil, simplemente ignorar
		return
	}
}

func (s *SemanticAnalyzer) analyzeExpression(expr Expression) string {
	// Verificar si la expresión es nil
	if expr == nil {
		return ""
	}
	
	switch e := expr.(type) {
	case *Identifier:
		if e == nil {
			return ""
		}
		if _, exists := s.lookupVariable(e.Name); !exists {
			s.addError(fmt.Sprintf("Variable '%s' no está declarada", e.Name), e.Line, e.Column)
			return ""
		}
		varInfo, _ := s.lookupVariable(e.Name)
		return varInfo.Type
	case *Literal:
		if e == nil {
			return ""
		}
		switch e.Type {
		case "INTEGER_LITERAL":
			return "int"
		case "FLOAT_LITERAL":
			return "double"
		case "STRING_LITERAL":
			return "String"
		}
		return ""
	case *BinaryExpression:
		if e == nil {
			return ""
		}
		leftType := s.analyzeExpression(e.Left)
		rightType := s.analyzeExpression(e.Right)
		return s.checkBinaryOperation(e.Operator, leftType, rightType, e.Line, e.Column)
	case *UnaryExpression:
		if e == nil {
			return ""
		}
		operandType := s.analyzeExpression(e.Operand)
		return s.checkUnaryOperation(e.Operator, operandType, e.Line, e.Column)
	case *Assignment:
		if e == nil {
			return ""
		}
		leftType := s.analyzeExpression(e.Left)
		rightType := s.analyzeExpression(e.Right)
		if leftType != "" && rightType != "" && !s.isAssignableType(leftType, rightType) {
			s.addError(fmt.Sprintf("No se puede asignar %s a %s", rightType, leftType), e.Line, e.Column)
		}
		return leftType
	case *MethodCall:
		if e == nil {
	if e != nil {
		if ident, ok := e.Object.(*FieldAccess); ok {
			if left, ok := ident.Object.(*Identifier); ok && left.Name == "system" {
				if ident.Field == "out" && e.Method == "printl" {
					s.addWarning("¿Quizás quisiste escribir 'println' en vez de 'printl'?", e.Line, e.Column)
				}
			}
		}
	}

			return ""
		}
		if e.Object != nil {
			s.analyzeExpression(e.Object)
		}
		for _, arg := range e.Arguments {
			if arg != nil {
				s.analyzeExpression(arg)
			}
		}
		// Simplified method analysis
		return "Object"
	case *FieldAccess:
		if e == nil {
			return ""
		}
		if e.Object != nil {
			s.analyzeExpression(e.Object)
		}
		return "Object"
	default:
		// Expresión desconocida o nil
		return ""
	}
}

func (s *SemanticAnalyzer) checkBinaryOperation(op, leftType, rightType string, line, column int) string {
	if leftType == "" || rightType == "" {
		return ""
	}
	
	switch op {
	case "+", "-", "*", "/", "%":
		if isNumericType(leftType) && isNumericType(rightType) {
			if leftType == "double" || rightType == "double" {
				return "double"
			}
			return "int"
		}
		if op == "+" && (leftType == "String" || rightType == "String") {
			return "String"
		}
		s.addError(fmt.Sprintf("Operador '%s' no aplicable a tipos %s y %s", op, leftType, rightType), line, column)
		return ""
	case "==", "!=", "<", ">", "<=", ">=":
		if isNumericType(leftType) && isNumericType(rightType) {
			return "boolean"
		}
		s.addError(fmt.Sprintf("Operador '%s' no aplicable a tipos %s y %s", op, leftType, rightType), line, column)
		return ""
	case "&&", "||":
		if leftType == "boolean" && rightType == "boolean" {
			return "boolean"
		}
		s.addError(fmt.Sprintf("Operador '%s' requiere operandos booleanos", op), line, column)
		return ""
	}
	return ""
}

func (s *SemanticAnalyzer) checkUnaryOperation(op, operandType string, line, column int) string {
	if operandType == "" {
		return ""
	}
	
	switch op {
	case "+", "-":
		if isNumericType(operandType) {
			return operandType
		}
		s.addError(fmt.Sprintf("Operador '%s' no aplicable a tipo %s", op, operandType), line, column)
		return ""
	case "!":
		if operandType == "boolean" {
			return "boolean"
		}
		s.addError(fmt.Sprintf("Operador '%s' requiere operando booleano", op), line, column)
		return ""
	}
	return ""
}

func (s *SemanticAnalyzer) isAssignableType(target, source string) bool {
	if target == source {
		return true
	}
	
	// Simple type promotion rules
	if target == "double" && isNumericType(source) {
		return true
	}
	if target == "float" && (source == "int" || source == "byte" || source == "short") {
		return true
	}
	if target == "long" && (source == "int" || source == "byte" || source == "short") {
		return true
	}
	if target == "int" && (source == "byte" || source == "short") {
		return true
	}
	
	return false
}

// Helper functions

func isValidType(t string) bool {
	validTypes := []string{
		"void", "boolean", "byte", "char", "short", "int", "long", "float", "double",
		"String", "Object",
	}
	for _, valid := range validTypes {
		if t == valid {
			return true
		}
	}
	return false
}


func isNumericType(t string) bool {
	numericTypes := []string{"byte", "short", "int", "long", "float", "double"}
	for _, numeric := range numericTypes {
		if t == numeric {
			return true
		}
	}
	return false
}

func isCapitalized(name string) bool {
	if len(name) == 0 {
		return false
	}
	return unicode.IsUpper(rune(name[0]))
}

func isLowerCamelCase(name string) bool {
	if len(name) == 0 {
		return false
	}
	return unicode.IsLower(rune(name[0]))
}

// CORS middleware
func enableCORS(w http.ResponseWriter, r *http.Request) {
	w.Header().Set("Access-Control-Allow-Origin", "*")
	w.Header().Set("Access-Control-Allow-Methods", "POST, GET, OPTIONS, PUT, DELETE")
	w.Header().Set("Access-Control-Allow-Headers", "Accept, Content-Type, Content-Length, Accept-Encoding, X-CSRF-Token, Authorization")
}

// API Handler
func analyzeHandler(w http.ResponseWriter, r *http.Request) {
	enableCORS(w, r)
	
	if r.Method == "OPTIONS" {
		w.WriteHeader(http.StatusOK)
		return
	}
	
	if r.Method != "POST" {
		http.Error(w, "Método no permitido", http.StatusMethodNotAllowed)
		return
	}
	
	var request struct {
		Input string `json:"input"`
	}
	
	if err := json.NewDecoder(r.Body).Decode(&request); err != nil {
		http.Error(w, "JSON inválido", http.StatusBadRequest)
		return
	}
	
	// Análisis léxico
	lexer := NewLexer(request.Input)
	tokens, invalidTokens := lexer.Tokenize()
	
	// Análisis sintáctico
	parser := NewParser(tokens)
	program, parseErrors, parseWarnings := parser.Parse()
	
	// Verificar si el programa es nil
	if program == nil {
		program = &Program{} // Crear un programa vacío para evitar nil pointer
	}
	
	// Análisis semántico
	semanticAnalyzer := NewSemanticAnalyzer()
	semanticErrors, semanticWarnings := semanticAnalyzer.Analyze(program)
	
	// Combinar errores y warnings
	allErrors := append(parseErrors, semanticErrors...)
	allWarnings := append(parseWarnings, semanticWarnings...)
	
	output := Output{
		Tokens:               tokens,
		TokensInvalidos:      invalidTokens,
		TotalTokens:          len(tokens),
		TotalTokensInvalidos: len(invalidTokens),
		Errores:              allErrors,
		Warnings:             allWarnings,
	}
	
	w.Header().Set("Content-Type", "application/json")
	json.NewEncoder(w).Encode(output)
}

func main() {
	http.HandleFunc("/analyze", analyzeHandler)
	
	fmt.Println("Servidor iniciado en puerto 8000")
	fmt.Println("Endpoint: POST http://localhost:8000/analyze")
	fmt.Println("Payload: {\"input\": \"código Java aquí\"}")
	
	log.Fatal(http.ListenAndServe(":8000", nil))
}