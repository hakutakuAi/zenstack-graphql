import { BaseGenerator } from './base-generator'
import { ErrorCategory, warning } from '@utils/error'
import { DataModel, Enum } from '@zenstackhq/sdk/ast'

export interface GenerationResult<T = string> {
	success: boolean
	result?: T
	error?: Error
}

export abstract class AbstractGenerator<T = string[]> extends BaseGenerator<T> {
	protected safeGenerate<R>(
		operation: () => R,
		context: {
			itemName: string
			itemType: string
			operationType: string
		},
	): GenerationResult<R> {
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
			})

			return {
				success: false,
				error: error instanceof Error ? error : new Error(String(error)),
			}
		}
	}

	protected processItems<TItem, TResult>(
		items: TItem[],
		processor: (item: TItem) => GenerationResult<TResult>,
		filter?: (item: TItem) => boolean,
	): TResult[] {
		const filteredItems = filter ? items.filter(filter) : items
		const results: TResult[] = []

		for (const item of filteredItems) {
			const result = processor(item)
			if (result.success && result.result !== undefined) {
				results.push(result.result)
			}
		}

		return results
	}

	protected processModels<TResult>(processor: (model: DataModel) => GenerationResult<TResult>): TResult[] {
		return this.processItems(this.models, processor, (model) => !this.attributeProcessor.model(model).isIgnored())
	}

	protected processEnums<TResult>(processor: (enumType: Enum) => GenerationResult<TResult>): TResult[] {
		return this.processItems(this.enums, processor)
	}

	protected getFormattedName(name: string): string {
		return this.typeFormatter.formatTypeName(name)
	}

	protected getFormattedFieldName(name: string): string {
		return this.typeFormatter.formatFieldName(name)
	}
}
