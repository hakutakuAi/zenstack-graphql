export enum ErrorCategory {
	VALIDATION = 'VALIDATION',
	SCHEMA = 'SCHEMA',
	FILE = 'FILE',
	GENERATION = 'GENERATION',
	PLUGIN = 'PLUGIN',
}

export type PluginErrorContext = Record<string, unknown>

export class PluginError extends Error {
	public readonly category: ErrorCategory
	public readonly context?: Record<string, unknown>
	public readonly suggestions?: string[]

	constructor(message: string, category: ErrorCategory, context?: Record<string, unknown>, suggestions?: string[]) {
		const formattedMessage = formatErrorMessage({
			message,
			category,
			context,
			suggestions,
		})

		super(formattedMessage)
		this.name = 'PluginError'
		this.category = category
		this.context = context
		this.suggestions = suggestions
	}
}

interface ErrorMessageData {
	message: string
	category: ErrorCategory
	context?: PluginErrorContext
	suggestions?: string[]
}

function formatErrorMessage(errorData: ErrorMessageData): string {
	let formattedMessage = `[${errorData.category}] ${errorData.message}`

	if (errorData.context && Object.keys(errorData.context).length > 0) {
		formattedMessage += '\n\nContext:'
		for (const [key, value] of Object.entries(errorData.context)) {
			const formattedValue = typeof value === 'object' && value !== null ? JSON.stringify(value, null, 2) : value
			formattedMessage += `\n- ${key}: ${formattedValue}`
		}
	}

	if (errorData.suggestions && errorData.suggestions.length > 0) {
		formattedMessage += '\n\nSuggestions:'
		for (const suggestion of errorData.suggestions) {
			formattedMessage += `\n- ${suggestion}`
		}
	}

	return formattedMessage
}

export function warning(message: string, category: ErrorCategory, context?: PluginErrorContext): void {
	console.warn(formatErrorMessage({ message, category, context }))
}
