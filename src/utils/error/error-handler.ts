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

export class ErrorHandler {
	createError(message: string, category: ErrorCategory, severity: ErrorSeverity = ErrorSeverity.ERROR, context?: Record<string, unknown>, suggestions?: string[]): PluginError {
		return new PluginError(message, category, severity, context, suggestions)
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
