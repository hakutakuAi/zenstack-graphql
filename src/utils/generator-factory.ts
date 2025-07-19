import { SchemaComposer } from 'graphql-compose'
import { GeneratorContext } from '@types'
import { AttributeProcessor } from '@utils/schema/attribute-processor'
import { TypeMapper } from '@utils/schema/type-mapper'
import { ErrorHandler } from '@utils/error/error-handler'
import { NormalizedOptions } from '@utils/config/options-validator'
import { TypeFormatter } from '@utils/schema/type-formatter'
import { Registry } from '@/utils/registry/registry'
import { BaseGenerator } from '@generators/base-generator'
import { DataModel, Enum } from '@zenstackhq/sdk/ast'

type GeneratorConstructor<T extends BaseGenerator> = new (context: GeneratorContext) => T

export class GeneratorFactory {
	private readonly context: GeneratorContext

	constructor(params: {
		schemaComposer: SchemaComposer<unknown>
		options: NormalizedOptions
		errorHandler: ErrorHandler
		attributeProcessor: AttributeProcessor
		typeMapper: TypeMapper
		models: DataModel[]
		enums: Enum[]
		typeFormatter: TypeFormatter
		registry: Registry
	}) {
		this.context = {
			schemaComposer: params.schemaComposer,
			options: params.options,
			errorHandler: params.errorHandler,
			attributeProcessor: params.attributeProcessor,
			typeFormatter: params.typeFormatter,
			typeMapper: params.typeMapper,
			models: params.models,
			enums: params.enums,
			registry: params.registry,
		}
	}

	create<T extends BaseGenerator>(generatorClass: GeneratorConstructor<T>): T {
		return new generatorClass(this.context)
	}
}
