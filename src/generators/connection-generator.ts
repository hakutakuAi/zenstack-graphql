import { GeneratorContext } from '@types'
import { BaseGenerator } from '@generators/base-generator'
import { ValidationUtils } from '@utils/schema/validation'
import { TypeKind } from '@utils/registry/unified-registry'
import { GraphQLTypeFactories } from '@utils/schema/graphql-type-factories'
import { Generate, SchemaOp } from '@utils/error'
import { DataModel } from '@zenstackhq/sdk/ast'

export class ConnectionGenerator extends BaseGenerator {
	private models: DataModel[]
	private typeFactories: GraphQLTypeFactories

	constructor(context: GeneratorContext) {
		super(context)

		this.models = context.models
		this.typeFactories = new GraphQLTypeFactories(this.schemaComposer, this.typeFormatter)
	}

	protected override skipGeneration(): boolean {
		return !this.options.connectionTypes
	}

	@Generate({
		suggestions: ['Check model definitions in your schema', 'Ensure connection type options are properly configured', 'Verify Relay specification compliance'],
	})
	generate(): void {
		if (this.skipGeneration()) {
			return
		}

		this.createCommonTypes()
		this.models.filter((model) => ValidationUtils.shouldGenerateModel(model, this.attributeProcessor)).forEach((model) => this.generateConnectionType(model))
	}

	@SchemaOp()
	private createCommonTypes(): void {
		const pageInfoTC = this.typeFactories.createPageInfoType()
		this.registry.registerType('PageInfo', TypeKind.OBJECT, pageInfoTC, true)

		const paginationInputTypes = this.typeFactories.createPaginationInputTypes()
		paginationInputTypes.forEach((inputTC) => {
			this.registry.registerType(inputTC.getTypeName(), TypeKind.INPUT, inputTC, true)
		})
	}

	@Generate({
		suggestions: ['Check model definition in schema', 'Verify all field types are supported for connection types'],
	})
	private generateConnectionType(model: DataModel): void {
		const typeName = this.getObjectTypeName(model)
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
	}

	private getObjectTypeName(model: DataModel): string {
		const customName = ValidationUtils.getModelName(model, this.attributeProcessor)
		return this.typeFormatter.formatTypeName(customName || model.name)
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
