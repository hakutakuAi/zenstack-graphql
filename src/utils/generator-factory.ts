import { SchemaComposer } from 'graphql-compose'
import { GeneratorContext } from '@types'
import { AttributeProcessor } from '@utils/schema/attribute-processor'
import { TypeMapper } from '@utils/schema/type-mapper'
import { ErrorHandler } from '@utils/error/error-handler'
import { NormalizedOptions } from '@utils/config/options-validator'
import { TypeFormatter } from '@utils/schema/type-formatter'
import type { DMMF } from '@prisma/generator-helper'
import { UnifiedRegistry } from '@utils/registry/unified-registry'

import { ObjectTypeGenerator } from '@generators/object-type-generator'
import { EnumGenerator } from '@generators/enum-generator'
import { ScalarGenerator } from '@generators/scalar-generator'
import { RelationGenerator } from '@generators/relation-generator'
import { ConnectionGenerator } from '@generators/connection-generator'
import { SortInputGenerator } from '@generators/sort-input-generator'
import { FilterInputGenerator } from '@generators/filter-input-generator'
import { DataModel, Enum } from '@zenstackhq/sdk/ast'

export class GeneratorFactory {
	private readonly context: GeneratorContext

	constructor(params: {
		schemaComposer: SchemaComposer<unknown>
		options: NormalizedOptions
		errorHandler: ErrorHandler
		attributeProcessor: AttributeProcessor
		typeMapper?: TypeMapper
		models?: DataModel[]
		enums?: Enum[]
		typeFormatter?: TypeFormatter
		registry?: UnifiedRegistry
	}) {
		const registry = params.registry || new UnifiedRegistry(params.schemaComposer, params.errorHandler)

		const models = params.models || []
		const enums = params.enums || []

		const typeMapper = params.typeMapper || new TypeMapper(models, enums)

		this.context = {
			schemaComposer: params.schemaComposer,
			options: params.options,
			errorHandler: params.errorHandler,
			attributeProcessor: params.attributeProcessor,
			typeFormatter: params.typeFormatter || TypeFormatter.fromOptions(params.options.typeNaming, params.options.fieldNaming),
			typeMapper: typeMapper,
			models: models,
			enums: enums,
			registry: registry,
		}
	}

	private validateContext(requiredProps: Array<keyof GeneratorContext>): void {
		for (const prop of requiredProps) {
			if (this.context[prop] === undefined) {
				throw new Error(`${String(prop)} is required for this generator but was not provided`)
			}
		}
	}

	createObjectTypeGenerator(): ObjectTypeGenerator {
		this.validateContext(['typeMapper', 'models'])
		return new ObjectTypeGenerator(this.context)
	}

	createScalarGenerator(): ScalarGenerator {
		this.validateContext(['typeMapper'])
		return new ScalarGenerator(this.context)
	}

	createEnumGenerator(): EnumGenerator {
		this.validateContext(['enums'])
		return new EnumGenerator(this.context)
	}

	createRelationGenerator(): RelationGenerator {
		this.validateContext(['typeMapper', 'models'])
		return new RelationGenerator(this.context)
	}

	createConnectionGenerator(): ConnectionGenerator {
		this.validateContext(['typeMapper', 'models'])
		return new ConnectionGenerator(this.context)
	}

	createSortInputGenerator(): SortInputGenerator {
		this.validateContext(['typeMapper', 'models'])
		return new SortInputGenerator(this.context)
	}

	createFilterInputGenerator(): FilterInputGenerator {
		this.validateContext(['typeMapper', 'models'])
		return new FilterInputGenerator(this.context)
	}
}
