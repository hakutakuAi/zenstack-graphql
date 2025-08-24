import { DataModel } from '@zenstackhq/sdk/ast'
import { ErrorCategory, handleError } from '@utils/error'
import { OutputStrategy, UnifiedGeneratorContext } from '@generators/strategies'
import { TypeFormatter } from '@utils/schema/type-formatter'
import { SchemaProcessor } from '@utils/schema/schema-processor'
import { UnifiedTypeMapper } from '@utils/type-mapping/unified-type-mapper'
import { NormalizedOptions } from '@/utils'

export abstract class UnifiedGeneratorBase {
	protected outputStrategy: OutputStrategy
	protected options: NormalizedOptions
	protected models: DataModel[]
	protected enums: any[]
	protected typeFormatter: TypeFormatter
	protected attributeProcessor: SchemaProcessor
	protected typeMapper?: UnifiedTypeMapper

	constructor(context: UnifiedGeneratorContext) {
		this.outputStrategy = context.outputStrategy
		this.options = context.options
		this.models = context.models
		this.enums = context.enums
		this.typeFormatter = context.typeFormatter
		this.attributeProcessor = context.attributeProcessor
		this.typeMapper = context.typeMapper
	}

	generate(): string[] {
		this.beforeGeneration()
		const results = this.generateForAllModels()
		this.afterGeneration()
		return this.processResults(results)
	}

	protected beforeGeneration(): void {
		this.createCommonTypes()
	}

	protected afterGeneration(): void {}

	protected processResults(results: string[]): string[] {
		return results.length > 0 ? results : this.outputStrategy.getGeneratedTypeNames()
	}

	protected generateForAllModels(): string[] {
		const results: string[] = []

		this.forEachValidModel((model) => {
			try {
				const result = this.generateForModel(model)
				if (result) {
					results.push(result)
				}
			} catch (error) {
				this.handleGenerationError(error, model)
			}
		})

		return results
	}

	protected abstract generateForModel(model: DataModel): string | null

	protected createCommonTypes(): void {}

	protected handleGenerationError(error: unknown, model: DataModel): void {
		const generatorType = this.constructor.name
		const errorMessage = `${generatorType} failed for model ${model.name}`
		handleError(error, errorMessage, {
			generatorType,
			modelName: model.name,
			model: model.name,
		})
	}

	protected getFormattedTypeName(model: DataModel): string {
		return this.attributeProcessor.model(model).getFormattedTypeName(this.typeFormatter)
	}

	protected shouldSkipModel(model: DataModel): boolean {
		return this.attributeProcessor.model(model).isIgnored()
	}

	protected getValidSortableFields(model: DataModel) {
		return model.fields.filter((field) => {
			const fieldProcessor = this.attributeProcessor.field(model, field.name)
			return fieldProcessor.isSortable() && fieldProcessor.isSortableType() && fieldProcessor.shouldInclude(true)
		})
	}

	protected getValidFilterableFields(model: DataModel) {
		return model.fields.filter((field) => {
			const fieldProcessor = this.attributeProcessor.field(model, field.name)
			return fieldProcessor.isFilterable() && fieldProcessor.shouldInclude(true)
		})
	}

	protected hasType(typeName: string): boolean {
		return this.outputStrategy.hasType(typeName)
	}

	protected forEachValidModel(callback: (model: DataModel) => void): void {
		this.models.filter((model) => !model.isAbstract && !this.shouldSkipModel(model)).forEach(callback)
	}
}
