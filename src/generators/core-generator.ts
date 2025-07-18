import { SchemaComposer } from 'graphql-compose'
import type { Model } from '@zenstackhq/sdk/ast'
import type { DMMF } from '@zenstackhq/sdk/prisma'
import { ErrorHandler, GenerationError } from '@utils/error-handler'
import { AttributeProcessor } from '@utils/attribute-processor'
import { TypeMapper } from '@utils/type-mapper'
import { SchemaBuilder } from '@utils/schema-builder'
import { NormalizedOptions } from '@utils/options-validator'
import { EnumGenerator } from '@generators/enum-generator'
import { ScalarGenerator } from '@generators/scalar-generator'
import { ObjectTypeGenerator } from '@generators/object-type-generator'
import { RelationGenerator } from '@generators/relation-generator'
import { ConnectionGenerator } from '@generators/connection-generator'

export interface GenerationStats {
	objectTypes: number
	enumTypes: number
	scalarTypes: number
	relationFields: number
	connectionTypes: number
	warnings: string[]
}

export interface GenerationResult {
	sdl: string
	stats: GenerationStats
}

export interface CoreGeneratorContext {
	model: Model
	options: NormalizedOptions
	dmmf: DMMF.Document
	errorHandler?: ErrorHandler
	attributeProcessor?: AttributeProcessor
	typeMapper?: TypeMapper
	schemaComposer?: SchemaComposer<unknown>
}

export class CoreGenerator {
	private schemaComposer: SchemaComposer<unknown>
	private errorHandler: ErrorHandler
	private attributeProcessor: AttributeProcessor
	private typeMapper: TypeMapper
	private options: NormalizedOptions
	private model: Model
	private dmmf: DMMF.Document
	private schemaBuilder: SchemaBuilder
	private warnings: string[] = []

	constructor(context: CoreGeneratorContext) {
		this.model = context.model
		this.options = context.options
		this.dmmf = context.dmmf
		this.errorHandler = context.errorHandler || ErrorHandler.getInstance()
		this.attributeProcessor = context.attributeProcessor || new AttributeProcessor()
		this.typeMapper = context.typeMapper || TypeMapper.createFromDMMF(context.dmmf)
		this.schemaComposer = context.schemaComposer || new SchemaComposer()
		this.schemaBuilder = new SchemaBuilder(this.schemaComposer, this.errorHandler)
	}

	generateSchema(): GenerationResult {
		try {
			if (this.options.relayCompliant) {
				this.schemaBuilder.addRelayRequirements()
			}

			const scalarGenerator = this.createScalarGenerator()
			scalarGenerator.generate()
			const scalarTypes = scalarGenerator.getRegisteredScalars()

			const enumGenerator = this.createEnumGenerator()
			enumGenerator.generate()
			const enumTypes = enumGenerator.getGeneratedEnums()

			const objectGenerator = this.createObjectTypeGenerator()
			objectGenerator.generate()
			const objectTypes = objectGenerator.getGeneratedTypes()

			const relationGenerator = this.createRelationGenerator()
			relationGenerator.generate()
			const relationFields = relationGenerator.getProcessedRelations()

			let connectionTypes: string[] = []
			if (this.options.connectionTypes) {
				const connectionGenerator = this.createConnectionGenerator()
				connectionGenerator.generate()
				connectionTypes = connectionGenerator.getGeneratedConnectionTypes()
			}

			const validationErrors = this.schemaBuilder.validateSchema()
			if (validationErrors.length > 0) {
				this.warnings.push(...validationErrors)
			}

			const sdl = this.schemaBuilder.generateSDL()

			return {
				sdl,
				stats: {
					objectTypes: objectTypes.length,
					enumTypes: enumTypes.length,
					scalarTypes: scalarTypes.length,
					relationFields: relationFields.length,
					connectionTypes: connectionTypes.length,
					warnings: this.warnings,
				},
			}
		} catch (error) {
			if (error instanceof GenerationError) {
				throw error
			}

			throw this.errorHandler.createGenerationError('Failed to generate GraphQL schema', { originalError: error }, [
				'Check for errors in your ZModel schema',
				'Verify that all referenced models and fields exist',
				'Ensure proper configuration of plugin options',
			])
		}
	}

	private createScalarGenerator(): ScalarGenerator {
		return new ScalarGenerator({
			schemaComposer: this.schemaComposer,
			options: this.options,
			errorHandler: this.errorHandler,
		})
	}

	private createEnumGenerator(): EnumGenerator {
		return new EnumGenerator({
			schemaComposer: this.schemaComposer,
			options: this.options,
			errorHandler: this.errorHandler,
			attributeProcessor: this.attributeProcessor,
			dmmfEnums: this.dmmf.datamodel.enums,
		})
	}

	private createObjectTypeGenerator(): ObjectTypeGenerator {
		return new ObjectTypeGenerator({
			schemaComposer: this.schemaComposer,
			options: this.options,
			errorHandler: this.errorHandler,
			attributeProcessor: this.attributeProcessor,
			typeMapper: this.typeMapper,
			dmmfModels: this.dmmf.datamodel.models,
		})
	}

	private createRelationGenerator(): RelationGenerator {
		return new RelationGenerator({
			schemaComposer: this.schemaComposer,
			options: this.options,
			errorHandler: this.errorHandler,
			attributeProcessor: this.attributeProcessor,
			typeMapper: this.typeMapper,
			dmmfModels: this.dmmf.datamodel.models,
		})
	}

	private createConnectionGenerator(): ConnectionGenerator {
		return new ConnectionGenerator({
			schemaComposer: this.schemaComposer,
			options: this.options,
			errorHandler: this.errorHandler,
			attributeProcessor: this.attributeProcessor,
			typeMapper: this.typeMapper,
			dmmfModels: this.dmmf.datamodel.models,
		})
	}

	addWarning(message: string): void {
		this.warnings.push(message)
	}

	getWarnings(): string[] {
		return this.warnings
	}
}
