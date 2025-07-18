import { SchemaComposer } from 'graphql-compose'
import { BaseGeneratorContext, ModelBasedGeneratorContext, EnumGeneratorContext, TypedGeneratorContext } from '@types'
import { AttributeProcessor } from '@utils/attribute-processor'
import { TypeMapper } from '@utils/type-mapper'
import { ErrorHandler } from '@utils/error-handler'
import { NormalizedOptions } from '@utils/options-validator'
import { DMMF } from '@zenstackhq/sdk/prisma'

import { ObjectTypeGenerator } from '@generators/object-type-generator'
import { EnumGenerator } from '@generators/enum-generator'
import { ScalarGenerator } from '@generators/scalar-generator'
import { RelationGenerator } from '@generators/relation-generator'
import { ConnectionGenerator } from '@generators/connection-generator'

export class GeneratorFactory {
	private readonly schemaComposer: SchemaComposer<unknown>
	private readonly options: NormalizedOptions
	private readonly errorHandler: ErrorHandler
	private readonly attributeProcessor: AttributeProcessor
	private readonly typeMapper?: TypeMapper
	private readonly dmmfModels?: readonly DMMF.Model[]
	private readonly dmmfEnums?: readonly DMMF.DatamodelEnum[]

	constructor(params: {
		schemaComposer: SchemaComposer<unknown>
		options: NormalizedOptions
		errorHandler: ErrorHandler
		attributeProcessor: AttributeProcessor
		typeMapper?: TypeMapper
		dmmfModels?: readonly DMMF.Model[]
		dmmfEnums?: readonly DMMF.DatamodelEnum[]
	}) {
		this.schemaComposer = params.schemaComposer
		this.options = params.options
		this.errorHandler = params.errorHandler
		this.attributeProcessor = params.attributeProcessor
		this.typeMapper = params.typeMapper
		this.dmmfModels = params.dmmfModels
		this.dmmfEnums = params.dmmfEnums
	}

	private createBaseContext(): BaseGeneratorContext {
		return {
			schemaComposer: this.schemaComposer,
			options: this.options,
			errorHandler: this.errorHandler,
			attributeProcessor: this.attributeProcessor,
		}
	}

	private createTypedContext(): TypedGeneratorContext {
		if (!this.typeMapper) {
			throw new Error('TypeMapper is required for creating typed generators')
		}

		return {
			...this.createBaseContext(),
			typeMapper: this.typeMapper,
		}
	}

	private createModelBasedContext(): ModelBasedGeneratorContext {
		if (!this.dmmfModels) {
			throw new Error('DMMF models are required for creating model-based generators')
		}

		return {
			...this.createTypedContext(),
			dmmfModels: this.dmmfModels,
		}
	}

	private createEnumContext(): EnumGeneratorContext {
		if (!this.dmmfEnums) {
			throw new Error('DMMF enums are required for creating enum generators')
		}

		return {
			...this.createBaseContext(),
			dmmfEnums: this.dmmfEnums,
		}
	}

	createObjectTypeGenerator(): ObjectTypeGenerator {
		return new ObjectTypeGenerator(this.createModelBasedContext())
	}

	createScalarGenerator(): ScalarGenerator {
		return new ScalarGenerator(this.createTypedContext())
	}

	createEnumGenerator(): EnumGenerator {
		return new EnumGenerator(this.createEnumContext())
	}

	createRelationGenerator(): RelationGenerator {
		return new RelationGenerator(this.createModelBasedContext())
	}

	createConnectionGenerator(): ConnectionGenerator {
		return new ConnectionGenerator(this.createModelBasedContext())
	}
}
