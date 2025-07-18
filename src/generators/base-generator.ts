import { SchemaComposer } from 'graphql-compose'
import { ErrorHandler } from '@utils/error-handler'
import { NormalizedOptions } from '@utils/options-validator'
import { AttributeProcessor } from '@utils/attribute-processor'
import { TypeMapper } from '@utils/type-mapper'
import { formatFieldName, formatTypeName } from '@utils/string-utils'
import { ComposerType } from '@/types'

export abstract class BaseGenerator<T extends ComposerType = ComposerType> {
	protected readonly registeredItems: Set<string> = new Set()

	constructor(
		protected readonly schemaComposer: SchemaComposer<unknown>,
		protected readonly options: NormalizedOptions,
		protected readonly errorHandler: ErrorHandler,
		protected readonly attributeProcessor: AttributeProcessor,
		protected readonly typeMapper?: TypeMapper
	) {}

	abstract generate(): void

	getGeneratedItems(): string[] {
		return Array.from(this.registeredItems)
	}

	hasItem(name: string): boolean {
		return this.registeredItems.has(name) || this.schemaComposer.has(name)
	}

	protected registerItem(name: string): void {
		this.registeredItems.add(name)
	}

	protected formatTypeName(name: string): string {
		return formatTypeName(name, this.options.typeNaming)
	}

	protected formatFieldName(name: string): string {
		return formatFieldName(name, this.options.fieldNaming)
	}

	protected handleError(operation: string, error: unknown, suggestions?: string[]): never {
		const operationContext = `${this.constructor.name}.${operation}`

		if (!suggestions) {
			suggestions = [`Check input data for ${operation}`, 'Verify schema definitions are correct', 'Ensure all required dependencies are available']
		}

		const errorMessage = `Error in ${operationContext}: ${error instanceof Error ? error.message : String(error)}`
		const context = { operation: operationContext, originalError: error }

		if (error instanceof Error) {
			if (error.message.toLowerCase().includes('validation') || error.message.toLowerCase().includes('invalid')) {
				throw this.errorHandler.createValidationError(errorMessage, context, suggestions)
			} else if (error.message.toLowerCase().includes('schema') || operation.toLowerCase().includes('schema')) {
				throw this.errorHandler.createSchemaError(errorMessage, context, suggestions)
			} else if (error.message.toLowerCase().includes('file') || operation.toLowerCase().includes('file')) {
				throw this.errorHandler.createFileError(errorMessage, context, suggestions)
			}
		}

		throw this.errorHandler.createGenerationError(errorMessage, context, suggestions)
	}
}
