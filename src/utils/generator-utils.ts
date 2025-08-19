import { ErrorCategory, warning } from '@utils/error'

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
