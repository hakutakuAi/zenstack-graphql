import { SchemaComposer } from 'graphql-compose'
import { GeneratorContext } from '@types'
import { AttributeProcessor } from '@utils/schema/attribute-processor'
import { TypeMapper } from '@utils/schema/type-mapper'
import { ErrorHandler } from '@utils/error/error-handler'
import { NormalizedOptions } from '@utils/config/options-validator'
import { TypeFormatter } from '@utils/schema/type-formatter'
import { DMMF } from '@zenstackhq/sdk/prisma'
import { UnifiedRegistry } from '@utils/registry/unified-registry'

import { ObjectTypeGenerator } from '@generators/object-type-generator'
import { EnumGenerator } from '@generators/enum-generator'
import { ScalarGenerator } from '@generators/scalar-generator'
import { RelationGenerator } from '@generators/relation-generator'
import { ConnectionGenerator } from '@generators/connection-generator'

export class GeneratorFactory {
	private readonly context: GeneratorContext

	constructor(params: {
		schemaComposer: SchemaComposer<unknown>
		options: NormalizedOptions
		errorHandler: ErrorHandler
		attributeProcessor: AttributeProcessor
		typeMapper?: TypeMapper
		dmmfModels?: readonly DMMF.Model[]
		dmmfEnums?: readonly DMMF.DatamodelEnum[]
		typeFormatter?: TypeFormatter
		registry?: UnifiedRegistry
	}) {
		this.context = {
			schemaComposer: params.schemaComposer,
			options: params.options,
			errorHandler: params.errorHandler,
			attributeProcessor: params.attributeProcessor,
			typeFormatter: params.typeFormatter || TypeFormatter.fromOptions(params.options.typeNaming, params.options.fieldNaming),
			typeMapper: params.typeMapper,
			dmmfModels: params.dmmfModels,
			dmmfEnums: params.dmmfEnums,
			registry: params.registry
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
		this.validateContext(['typeMapper', 'dmmfModels'])
		return new ObjectTypeGenerator(this.context)
	}

	createScalarGenerator(): ScalarGenerator {
		this.validateContext(['typeMapper'])
		return new ScalarGenerator(this.context)
	}

	createEnumGenerator(): EnumGenerator {
		this.validateContext(['dmmfEnums'])
		return new EnumGenerator(this.context)
	}

	createRelationGenerator(): RelationGenerator {
		this.validateContext(['typeMapper', 'dmmfModels'])
		return new RelationGenerator(this.context)
	}

	createConnectionGenerator(): ConnectionGenerator {
		this.validateContext(['typeMapper', 'dmmfModels'])
		return new ConnectionGenerator(this.context)
	}
}
