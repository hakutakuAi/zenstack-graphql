import { Result, ok, err } from 'neverthrow'

export enum ErrorCategory {
	VALIDATION = 'VALIDATION',
	SCHEMA = 'SCHEMA',
	FILE = 'FILE',
	GENERATION = 'GENERATION',
	PLUGIN = 'PLUGIN',
}

export type PluginErrorContext = Record<string, unknown>

export interface PluginErrorData {
	message: string
	category: ErrorCategory
	context?: PluginErrorContext
	suggestions?: string[]
}

export function formatErrorMessage(errorData: PluginErrorData): string {
	let formattedMessage = `[${errorData.category}] ${errorData.message}`

	if (errorData.context && Object.keys(errorData.context).length > 0) {
		formattedMessage += '\n\nContext:'
		for (const [key, value] of Object.entries(errorData.context)) {
			formattedMessage += `\n- ${key}: ${value}`
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

export function createError(message: string, category: ErrorCategory, context?: PluginErrorContext, suggestions?: string[]): Result<never, PluginErrorData> {
	return err({
		message,
		category,
		context,
		suggestions,
	})
}

export function logWarning(message: string, category: ErrorCategory, context?: PluginErrorContext): void {
	console.warn(formatErrorMessage({ message, category, context }))
}

export function fromThrowable<T, E extends PluginErrorData>(fn: () => T, errorMapper: (error: unknown) => E): Result<T, E> {
	try {
		return ok(fn())
	} catch (error) {
		return err(errorMapper(error))
	}
}

export type PluginResult<T> = Result<T, PluginErrorData>
