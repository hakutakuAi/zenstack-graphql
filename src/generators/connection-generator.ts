import { BaseGenerator } from '@generators/base-generator'
import { TypeKind } from '@/utils/registry/registry'
import { DataModel } from '@zenstackhq/sdk/ast'

export class ConnectionGenerator extends BaseGenerator {
	protected override skipGeneration(): boolean {
		return !this.options.connectionTypes
	}

	generate(): string[] {
		if (this.skipGeneration()) {
			return []
		}

		this.createCommonTypes()
		this.models.filter((model) => !this.attributeProcessor.model(model).isIgnored()).forEach((model) => this.generateConnectionType(model))

		return this.registry.getConnectionTypes()
	}

	private createCommonTypes(): void {
		const pageInfoTC = this.typeFactories.createPageInfoType()
		this.registry.registerType('PageInfo', TypeKind.OBJECT, pageInfoTC, true)

		const paginationInputTypes = this.typeFactories.createPaginationInputTypes()
		paginationInputTypes.forEach((inputTC) => {
			this.registry.registerType(inputTC.getTypeName(), TypeKind.INPUT, inputTC, true)
		})
	}

	private generateConnectionType(model: DataModel): void {
		const typeName = this.attributeProcessor.model(model).getFormattedTypeName(this.typeFormatter)
		const connectionName = this.typeFormatter.formatConnectionTypeName(typeName)

		if (this.registry.isTypeOfKind(connectionName, TypeKind.CONNECTION)) {
			return
		}

		if (!this.schemaComposer.has(typeName)) {
			const description = this.attributeProcessor.model(model).description()
			this.schemaComposer.createObjectTC({
				name: typeName,
				description,
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
}
