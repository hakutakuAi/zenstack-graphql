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

export function getErrorMessage(error: unknown): string {
	return error instanceof Error ? error.message : String(error)
}

export function getErrorCategory(error: unknown): ErrorCategory {
	return error instanceof PluginError ? error.category : ErrorCategory.GENERATION
}

export function handleError(error: unknown, operation: string, context?: PluginErrorContext): void {
	const message = getErrorMessage(error)
	const category = getErrorCategory(error)
	warning(`Failed to ${operation}: ${message}`, category, { ...context, error })
}

export interface SafeOperationContext {
	itemName: string
	itemType: string
	operationType: string
	additionalContext?: Record<string, any>
}

export interface SafeOperationResult<T> {
	success: boolean
	result?: T
	error?: Error
}

export function executeSafely<T>(operation: () => T, context: SafeOperationContext): SafeOperationResult<T> {
	try {
		const result = operation()
		return { success: true, result }
	} catch (error) {
		const errorMessage = `Failed to ${context.operationType} ${context.itemType} for ${context.itemName}: ${
			error instanceof Error ? error.message : String(error)
		}`

		warning(errorMessage, ErrorCategory.GENERATION, {
			itemName: context.itemName,
			itemType: context.itemType,
			operationType: context.operationType,
			error: error,
			...context.additionalContext,
		})

		return {
			success: false,
			error: error instanceof Error ? error : new Error(String(error)),
		}
	}
}

export function processArraySafely<TInput, TOutput>(
	items: TInput[],
	processor: (item: TInput) => SafeOperationResult<TOutput>,
	filter?: (item: TInput) => boolean,
): TOutput[] {
	const filteredItems = filter ? items.filter(filter) : items
	const results: TOutput[] = []

	for (const item of filteredItems) {
		const result = processor(item)
		if (result.success && result.result !== undefined) {
			results.push(result.result)
		}
	}

	return results
}

export function createGenerationContext(
	itemName: string,
	itemType: string,
	operationType: string,
	additionalContext?: Record<string, any>,
): SafeOperationContext {
	return {
		itemName,
		itemType,
		operationType,
		additionalContext,
	}
}
