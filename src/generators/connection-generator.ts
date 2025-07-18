import { SchemaComposer, ObjectTypeComposer } from 'graphql-compose'
import type { DMMF } from '@zenstackhq/sdk/prisma'
import { ErrorHandler } from '@utils/error-handler'
import { AttributeProcessor } from '@utils/attribute-processor'
import { TypeMapper } from '@utils/type-mapper'
import type { NormalizedOptions } from '@utils/options-validator'
import { ValidationUtils } from '@utils/validation'
import { BaseGenerator } from '@generators/base-generator'

export interface ConnectionGeneratorContext {
	schemaComposer: SchemaComposer
	options: NormalizedOptions
	errorHandler: ErrorHandler
	attributeProcessor: AttributeProcessor
	typeMapper: TypeMapper
	dmmfModels: readonly DMMF.Model[]
}

export interface ConnectionConfig {
	name: string
	nodeType: string
	description?: string
	edgeFields?: Record<string, unknown>
	connectionFields?: Record<string, unknown>
}

export class ConnectionGenerator extends BaseGenerator<ObjectTypeComposer<any, any>> {
	private dmmfModels: readonly DMMF.Model[]
	private generatedEdges: Set<string> = new Set()

	constructor(context: ConnectionGeneratorContext) {
		super(context.schemaComposer, context.options, context.errorHandler, context.attributeProcessor, context.typeMapper)
		this.dmmfModels = context.dmmfModels
	}

	generate(): void {
		if (!this.options.connectionTypes) {
			return
		}

		try {
			this.createCommonTypes()

			for (const dmmfModel of this.dmmfModels) {
				if (this.shouldGenerateConnection(dmmfModel)) {
					this.generateConnectionType(dmmfModel)
				}
			}
		} catch (error) {
			console.error('Original error in generate:', error)
			this.handleError('generate', error, ['Check model definitions in your schema', 'Ensure connection type options are properly configured', 'Verify Relay specification compliance'])
		}
	}

	private shouldGenerateConnection(dmmfModel: DMMF.Model): boolean {
		return ValidationUtils.shouldGenerateModel(dmmfModel, this.attributeProcessor)
	}

	private createCommonTypes(): void {
		this.createPageInfoType()
		this.createPaginationInputTypes()
		this.createSortDirectionEnum()
	}

	private createPageInfoType(): void {
		if (this.schemaComposer.has('PageInfo')) {
			return
		}

		try {
			const pageInfoTC = this.schemaComposer.createObjectTC({
				name: 'PageInfo',
				description: 'Information about pagination in a connection.',
				fields: {
					hasNextPage: {
						type: 'Boolean!',
						description: 'When paginating forwards, are there more items?',
					},
					hasPreviousPage: {
						type: 'Boolean!',
						description: 'When paginating backwards, are there more items?',
					},
					startCursor: {
						type: 'String',
						description: 'When paginating backwards, the cursor to continue.',
					},
					endCursor: {
						type: 'String',
						description: 'When paginating forwards, the cursor to continue.',
					},
				},
			})

			this.schemaComposer.set('PageInfo', pageInfoTC)
		} catch (error) {
			this.handleError('createPageInfoType', error, ['Check GraphQL type naming conflicts', 'Ensure PageInfo fields are valid GraphQL types'])
		}
	}

	private createPaginationInputTypes(): void {
		try {
			if (!this.schemaComposer.has('ForwardPaginationInput')) {
				const forwardPaginationInputTC = this.schemaComposer.createInputTC({
					name: 'ForwardPaginationInput',
					description: 'Pagination input for forward pagination',
					fields: {
						first: {
							type: 'Int',
							description: 'Returns the first n elements from the list.',
						},
						after: {
							type: 'String',
							description: 'Returns the elements in the list that come after the specified cursor.',
						},
					},
				})

				this.schemaComposer.set('ForwardPaginationInput', forwardPaginationInputTC)
			}

			if (!this.schemaComposer.has('BackwardPaginationInput')) {
				const backwardPaginationInputTC = this.schemaComposer.createInputTC({
					name: 'BackwardPaginationInput',
					description: 'Pagination input for backward pagination',
					fields: {
						last: {
							type: 'Int',
							description: 'Returns the last n elements from the list.',
						},
						before: {
							type: 'String',
							description: 'Returns the elements in the list that come before the specified cursor.',
						},
					},
				})

				this.schemaComposer.set('BackwardPaginationInput', backwardPaginationInputTC)
			}

			if (!this.schemaComposer.has('PaginationInput')) {
				const paginationInputTC = this.schemaComposer.createInputTC({
					name: 'PaginationInput',
					description: 'Combined pagination input',
					fields: {
						first: {
							type: 'Int',
							description: 'Returns the first n elements from the list.',
						},
						after: {
							type: 'String',
							description: 'Returns the elements in the list that come after the specified cursor.',
						},
						last: {
							type: 'Int',
							description: 'Returns the last n elements from the list.',
						},
						before: {
							type: 'String',
							description: 'Returns the elements in the list that come before the specified cursor.',
						},
					},
				})

				this.schemaComposer.set('PaginationInput', paginationInputTC)
			}
		} catch (error) {
			this.handleError('createPaginationInputTypes', error, ['Check GraphQL input type naming conflicts', 'Ensure input fields are valid GraphQL types'])
		}
	}

	private createSortDirectionEnum(): void {
		if (this.schemaComposer.has('SortDirection')) {
			return
		}

		try {
			const sortDirectionTC = this.schemaComposer.createEnumTC({
				name: 'SortDirection',
				description: 'Sort direction for ordering results',
				values: {
					ASC: {
						value: 'ASC',
						description: 'Ascending order',
					},
					DESC: {
						value: 'DESC',
						description: 'Descending order',
					},
				},
			})

			this.schemaComposer.set('SortDirection', sortDirectionTC)
		} catch (error) {
			this.handleError('createSortDirectionEnum', error, ['Check GraphQL enum naming conflicts'])
		}
	}

	private generateConnectionType(dmmfModel: DMMF.Model): void {
		try {
			const typeName = this.getObjectTypeName(dmmfModel)
			const connectionName = `${typeName}Connection`
			const edgeName = `${typeName}Edge`

			if (this.hasItem(connectionName)) {
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

			this.createEdgeType(typeName, edgeName, dmmfModel)

			const connectionTC = this.schemaComposer.createObjectTC({
				name: connectionName,
				description: `A connection to a list of ${typeName} items.`,
				fields: {
					pageInfo: {
						type: 'PageInfo!',
						description: 'Information to aid in pagination.',
					},
					edges: {
						type: `[${edgeName}!]!`,
						description: `A list of ${typeName} edges.`,
					},
					totalCount: {
						type: 'Int!',
						description: 'The total count of items in the connection.',
					},
				},
			})

			this.schemaComposer.set(connectionName, connectionTC)
			this.registerItem(connectionName)

			this.createSortInputType(dmmfModel)
		} catch (error) {
			this.handleError('generateConnectionType', error, ['Check model definition in schema', 'Verify all field types are supported for connection types'])
		}
	}

	private createEdgeType(typeName: string, edgeName: string, dmmfModel: DMMF.Model): void {
		if (this.generatedEdges.has(edgeName)) {
			return
		}

		try {
			const edgeTC = this.schemaComposer.createObjectTC({
				name: edgeName,
				description: `An edge in a ${typeName} connection.`,
				fields: {
					node: {
						type: `${typeName}!`,
						description: `The ${typeName} at the end of the edge.`,
					},
					cursor: {
						type: 'String!',
						description: 'A cursor for use in pagination.',
					},
				},
			})

			this.schemaComposer.set(edgeName, edgeTC)
			this.generatedEdges.add(edgeName)
		} catch (error) {
			this.handleError('createEdgeType', error, ['Check for naming conflicts', 'Ensure node type is defined in the schema'])
		}
	}

	private createSortInputType(dmmfModel: DMMF.Model): void {
		const typeName = this.getObjectTypeName(dmmfModel)
		const sortInputName = `${typeName}SortInput`

		if (this.schemaComposer.has(sortInputName)) {
			return
		}

		try {
			const fields: Record<string, { type: string; description: string }> = {}

			for (const field of dmmfModel.fields) {
				if (field.kind !== 'object' && ValidationUtils.shouldIncludeField(dmmfModel, field, this.attributeProcessor, true)) {
					const fieldName = this.formatFieldName(field.name)
					fields[fieldName] = {
						type: 'SortDirection',
						description: `Sort by ${fieldName}`,
					}
				}
			}

			if (Object.keys(fields).length > 0) {
				const sortInputTC = this.schemaComposer.createInputTC({
					name: sortInputName,
					description: `Sort input for ${typeName} connections`,
					fields,
				})

				this.schemaComposer.set(sortInputName, sortInputTC)
			}
		} catch (error) {
			this.handleError('createSortInputType', error, ['Check field definitions in the model', 'Ensure fields are valid for sorting'])
		}
	}

	private getObjectTypeName(dmmfModel: DMMF.Model): string {
		const customName = ValidationUtils.getModelName(dmmfModel, this.attributeProcessor)
		if (customName && customName !== dmmfModel.name) {
			return this.formatTypeName(customName)
		}
		return this.formatTypeName(dmmfModel.name)
	}

	getGeneratedConnectionTypes(): string[] {
		return this.getGeneratedItems()
	}

	getGeneratedEdgeTypes(): string[] {
		return Array.from(this.generatedEdges)
	}

	hasConnectionType(name: string): boolean {
		return this.hasItem(name)
	}

	hasEdgeType(name: string): boolean {
		return this.generatedEdges.has(name) || this.schemaComposer.has(name)
	}

	getConnectionTypeForModel(modelName: string): string | undefined {
		const formattedName = this.formatTypeName(modelName)
		const connectionName = `${formattedName}Connection`

		return this.hasConnectionType(connectionName) ? connectionName : undefined
	}
}
