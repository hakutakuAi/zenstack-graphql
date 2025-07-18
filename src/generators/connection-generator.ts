import { ObjectTypeComposer } from 'graphql-compose'
import { GeneratorContext, DMMF } from '@types'
import { BaseGenerator } from '@generators/base-generator'
import { ValidationUtils } from '@utils/schema/validation'
import { TypeKind } from '@utils/registry/unified-registry'
import { GraphQLTypeFactories } from '@utils/schema/graphql-type-factories'
import { Generate, SchemaOp, Validate } from '@utils/error'

export class ConnectionGenerator extends BaseGenerator {
	private dmmfModels: readonly DMMF.Model[]
	private typeFactories: GraphQLTypeFactories

	constructor(context: GeneratorContext) {
		super(context)
		if (!context.dmmfModels) {
			throw new Error('DMMF models are required for ConnectionGenerator')
		}
		if (!context.typeMapper) {
			throw new Error('TypeMapper is required for ConnectionGenerator')
		}
		this.dmmfModels = context.dmmfModels
		this.typeFactories = new GraphQLTypeFactories(this.schemaComposer, this.errorHandler, this.typeFormatter)
	}

	@Generate({
		suggestions: ['Check model definitions in your schema', 'Ensure connection type options are properly configured', 'Verify Relay specification compliance'],
	})
	generate(): void {
		if (!this.options.connectionTypes) {
			return
		}

		this.createCommonTypes()
		this.dmmfModels.filter((model) => ValidationUtils.shouldGenerateModel(model, this.attributeProcessor)).forEach((model) => this.generateConnectionType(model))
	}

	@Validate()
	private shouldIncludeField(dmmfModel: DMMF.Model, field: DMMF.Field): boolean {
		return ValidationUtils.shouldIncludeField(dmmfModel, field, this.attributeProcessor, true)
	}

	@SchemaOp()
	private createCommonTypes(): void {
		const pageInfoTC = this.typeFactories.createPageInfoType()
		this.registry.registerType('PageInfo', TypeKind.OBJECT, pageInfoTC, true)

		const paginationInputTypes = this.typeFactories.createPaginationInputTypes()
		paginationInputTypes.forEach((inputTC) => {
			this.registry.registerType(inputTC.getTypeName(), TypeKind.INPUT, inputTC, true)
		})

		const sortDirectionTC = this.typeFactories.createSortDirectionEnum()
		this.registry.registerType('SortDirection', TypeKind.ENUM, sortDirectionTC, true)
	}

	@Generate({
		suggestions: ['Check model definition in schema', 'Verify all field types are supported for connection types'],
	})
	private generateConnectionType(dmmfModel: DMMF.Model): void {
		const typeName = this.getObjectTypeName(dmmfModel)
		const connectionName = this.typeFormatter.formatConnectionTypeName(typeName)

		if (this.registry.isTypeOfKind(connectionName, TypeKind.CONNECTION)) {
			return
		}

		if (!this.schemaComposer.has(typeName)) {
			console.warn(`Base type ${typeName} does not exist in SchemaComposer. Creating a placeholder for connection generation.`)

			this.schemaComposer.createObjectTC({
				name: typeName,
				fields: {
					id: 'ID!',
				},
			})
		}

		const edgeName = this.typeFormatter.formatEdgeTypeName(typeName)
		const edgeTC = this.typeFactories.createEdgeType(typeName)
		this.registry.registerEdgeType(edgeName, edgeTC)

		const connectionTC = this.typeFactories.createConnectionType(typeName)
		this.registry.registerType(connectionName, TypeKind.CONNECTION, connectionTC, true)

		this.createSortInputType(dmmfModel)
	}

	@SchemaOp({
		suggestions: ['Check field definitions in the model', 'Ensure fields are valid for sorting'],
	})
	private createSortInputType(dmmfModel: DMMF.Model): void {
		const typeName = this.getObjectTypeName(dmmfModel)
		const sortInputName = this.typeFormatter.formatSortInputTypeName(typeName)

		if (this.schemaComposer.has(sortInputName)) {
			return
		}

		const fields = dmmfModel.fields
			.filter((field) => field.kind !== 'object' && this.shouldIncludeField(dmmfModel, field))
			.reduce(
				(acc, field) => {
					const fieldName = this.typeFormatter.formatFieldName(field.name)
					acc[fieldName] = { description: `Sort by ${fieldName}` }
					return acc
				},
				{} as Record<string, { description: string }>
			)

		const sortInputTC = this.typeFactories.createSortInputType(typeName, fields)
		this.registry.registerType(sortInputName, TypeKind.INPUT, sortInputTC, true)
	}

	private getObjectTypeName(dmmfModel: DMMF.Model): string {
		const customName = ValidationUtils.getModelName(dmmfModel, this.attributeProcessor)
		return this.typeFormatter.formatTypeName(customName || dmmfModel.name)
	}

	getGeneratedConnectionTypes(): string[] {
		return this.registry.getConnectionTypes()
	}

	getGeneratedEdgeTypes(): string[] {
		return this.registry.getEdgeTypes()
	}

	hasConnectionType(name: string): boolean {
		return this.registry.isTypeOfKind(name, TypeKind.CONNECTION)
	}

	hasEdgeType(name: string): boolean {
		return this.registry.hasEdgeType(name)
	}

	getConnectionTypeForModel(modelName: string): string | undefined {
		return this.registry.getConnectionTypeForModel(modelName, this.typeFormatter)
	}
}
