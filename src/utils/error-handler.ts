import { match } from 'ts-pattern'

export enum ErrorSeverity {
	FATAL = 'FATAL',
	ERROR = 'ERROR',
	WARNING = 'WARNING',
	INFO = 'INFO',
}

export enum ErrorCategory {
	VALIDATION = 'VALIDATION',
	SCHEMA = 'SCHEMA',
	FILE = 'FILE',
	GENERATION = 'GENERATION',
	PLUGIN = 'PLUGIN',
}

export class PluginError extends Error {
	severity: ErrorSeverity
	category: ErrorCategory
	context?: Record<string, unknown>
	suggestions?: string[]

	constructor(message: string, category: ErrorCategory, severity: ErrorSeverity = ErrorSeverity.ERROR, context?: Record<string, unknown>, suggestions?: string[]) {
		super(message)
		this.name = 'PluginError'
		this.severity = severity
		this.category = category
		this.context = context
		this.suggestions = suggestions
	}

	formatMessage(): string {
		let formattedMessage = `[${this.severity}] [${this.category}] ${this.message}`

		if (this.context && Object.keys(this.context).length > 0) {
			formattedMessage += '\n\nContext:'
			for (const [key, value] of Object.entries(this.context)) {
				formattedMessage += `\n- ${key}: ${value}`
			}
		}

		if (this.suggestions && this.suggestions.length > 0) {
			formattedMessage += '\n\nSuggestions:'
			for (const suggestion of this.suggestions) {
				formattedMessage += `\n- ${suggestion}`
			}
		}

		return formattedMessage
	}
}

export class ValidationError extends PluginError {
	constructor(message: string, context?: Record<string, unknown>, suggestions?: string[]) {
		super(message, ErrorCategory.VALIDATION, ErrorSeverity.ERROR, context, suggestions)
		this.name = 'ValidationError'
	}
}

export class SchemaError extends PluginError {
	constructor(message: string, context?: Record<string, unknown>, suggestions?: string[]) {
		super(message, ErrorCategory.SCHEMA, ErrorSeverity.ERROR, context, suggestions)
		this.name = 'SchemaError'
	}
}

export class FileError extends PluginError {
	constructor(message: string, context?: Record<string, unknown>, suggestions?: string[]) {
		super(message, ErrorCategory.FILE, ErrorSeverity.ERROR, context, suggestions)
		this.name = 'FileError'
	}
}

export class GenerationError extends PluginError {
	constructor(message: string, context?: Record<string, unknown>, suggestions?: string[]) {
		super(message, ErrorCategory.GENERATION, ErrorSeverity.ERROR, context, suggestions)
		this.name = 'GenerationError'
	}
}

export class ErrorHandler {
	handleError(error: unknown, context: string, suggestions?: string[]): never {
		const pluginError = this.wrapError(error, context, suggestions)
		console.error(pluginError.formatMessage())

		if (pluginError.severity === ErrorSeverity.FATAL) {
			console.error('Fatal error encountered. Plugin execution will terminate.')
		}

		throw pluginError
	}

	wrapError(error: unknown, context: string, suggestions?: string[]): PluginError {
		if (error instanceof PluginError) {
			if (context && (!error.context || !error.context.operationContext)) {
				error.context = { ...error.context, operationContext: context }
			}

			if (suggestions && (!error.suggestions || error.suggestions.length === 0)) {
				error.suggestions = suggestions
			}

			return error
		}

		const errorMessage = error instanceof Error ? error.message : String(error)
		const errorStack = error instanceof Error ? error.stack : undefined

		return new PluginError(
			errorMessage,
			ErrorCategory.PLUGIN,
			ErrorSeverity.ERROR,
			{
				operationContext: context,
				originalStack: errorStack,
			},
			suggestions
		)
	}

	createValidationError(message: string, context?: Record<string, unknown>, suggestions?: string[]): ValidationError {
		return new ValidationError(message, context, suggestions)
	}

	createSchemaError(message: string, context?: Record<string, unknown>, suggestions?: string[]): SchemaError {
		return new SchemaError(message, context, suggestions)
	}

	createFileError(message: string, context?: Record<string, unknown>, suggestions?: string[]): FileError {
		return new FileError(message, context, suggestions)
	}

	createGenerationError(message: string, context?: Record<string, unknown>, suggestions?: string[]): GenerationError {
		return new GenerationError(message, context, suggestions)
	}

	categorizeError(error: unknown, context: string): PluginError {
		const errorMessage = error instanceof Error ? error.message : String(error)

		return match(errorMessage.toLowerCase())
			.when(
				(msg) => msg.includes('option') || msg.includes('parameter') || msg.includes('config'),
				() => this.createValidationError(errorMessage, { operationContext: context })
			)
			.when(
				(msg) => msg.includes('schema') || msg.includes('model') || msg.includes('dmmf'),
				() => this.createSchemaError(errorMessage, { operationContext: context })
			)
			.when(
				(msg) => msg.includes('file') || msg.includes('directory') || msg.includes('path'),
				() => this.createFileError(errorMessage, { operationContext: context })
			)
			.when(
				(msg) => msg.includes('graphql') || msg.includes('type') || msg.includes('field'),
				() => this.createGenerationError(errorMessage, { operationContext: context })
			)
			.otherwise(() => this.wrapError(error, context))
	}

	logWarning(message: string, category: ErrorCategory, context?: Record<string, unknown>): void {
		const warning = new PluginError(message, category, ErrorSeverity.WARNING, context)
		console.warn(warning.formatMessage())
	}

	static getInstance(): ErrorHandler {
		if (!ErrorHandler.instance) {
			ErrorHandler.instance = new ErrorHandler()
		}
		return ErrorHandler.instance
	}

	private static instance: ErrorHandler
}

export default ErrorHandler.getInstance()
