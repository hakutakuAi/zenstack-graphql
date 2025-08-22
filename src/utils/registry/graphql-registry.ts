import {
	SchemaComposer,
	ObjectTypeComposer,
	EnumTypeComposer,
	ScalarTypeComposer,
	InputTypeComposer,
	InterfaceTypeComposer,
	UnionTypeComposer,
} from 'graphql-compose'
import { printSchema, GraphQLSchema } from 'graphql'
import { ErrorCategory, PluginError } from '@utils/error'
import { COMMON_TYPES } from '@utils/constants'
import { TypeKind, BaseTypeInfo } from './base-registry'
import { BaseRegistry } from './base-registry'

export interface GraphQLTypeInfo extends BaseTypeInfo<any> {
	composer: any
}

export class GraphQLRegistry extends BaseRegistry<any, GraphQLTypeInfo> {
	private readonly schemaComposer: SchemaComposer<unknown>
	private edgeTypes: Set<string> = new Set()
	private readonly composerToKindMap = new Map<any, TypeKind>([
		[ObjectTypeComposer, TypeKind.OBJECT],
		[ScalarTypeComposer, TypeKind.SCALAR],
		[EnumTypeComposer, TypeKind.ENUM],
		[InterfaceTypeComposer, TypeKind.INTERFACE],
		[InputTypeComposer, TypeKind.INPUT],
		[UnionTypeComposer, TypeKind.UNION],
	])

	constructor(schemaComposer: SchemaComposer<unknown>) {
		super()
		this.schemaComposer = schemaComposer
		this.syncFromSchemaComposer()
	}

	protected createTypeInfo(name: string, kind: TypeKind, composer: any, isGenerated: boolean): GraphQLTypeInfo {
		return {
			name,
			kind,
			description: this.getComposerDescription(composer),
			data: composer,
			composer,
			isGenerated,
		}
	}

	override registerType(typeName: string, kind: TypeKind, composer: any, isGenerated = true): void {
		super.registerType(typeName, kind, composer, isGenerated)
		this.schemaComposer.set(typeName, composer)
	}

	validateSchema(): string[] {
		const warnings: string[] = []

		try {
			this.buildSchema()
		} catch (error) {
			if (error instanceof Error) {
				warnings.push(`Schema validation failed: ${error.message}`)
			}
		}

		return warnings
	}

	buildSchema(): GraphQLSchema {
		try {
			return this.schemaComposer.buildSchema({ keepUnusedTypes: true })
		} catch (error) {
			if (error instanceof Error && error.message.includes('multiple types named')) {
				const duplicateMatch = error.message.match(/multiple types named "(\w+)"/)
				const duplicateType = duplicateMatch ? duplicateMatch[1] : 'unknown'

				throw new PluginError(
					`Failed to build GraphQL schema - duplicate type "${duplicateType}" detected`,
					ErrorCategory.SCHEMA,
					{
						originalError: error,
						duplicateType,
						registeredTypes: Array.from(this.schemaComposer.types.keys()),
						registryTypes: Array.from(this.types.keys()),
					},
					[
						`Remove duplicate registration of type "${duplicateType}"`,
						'Check scalar generator for duplicate registrations',
						'Verify type mapping configuration is correct',
					],
				)
			}

			throw new PluginError('Failed to build GraphQL schema', ErrorCategory.SCHEMA, { originalError: error }, [
				'Check for circular references in your schema',
				'Verify all required types are properly defined',
				'Ensure type names are unique across the schema',
			])
		}
	}

	generateSDL(): string {
		const schema = this.buildSchema()
		return printSchema(schema)
	}

	addRelayRequirements(): void {
		if (this.schemaComposer.has(COMMON_TYPES.NODE)) {
			return
		}
		const nodeInterface = this.schemaComposer.createInterfaceTC({
			name: COMMON_TYPES.NODE,
			description: 'An object with a unique identifier',
			fields: {
				id: {
					type: 'ID!',
					description: 'The unique identifier for this object',
				},
			},
		})
		this.registerType(COMMON_TYPES.NODE, TypeKind.INTERFACE, nodeInterface, true)
	}

	private syncFromSchemaComposer(): void {
		for (const typeName of this.schemaComposer.types.keys()) {
			const composer = this.schemaComposer.get(typeName)
			if (!composer) continue

			this.types.set(typeName, {
				name: typeName,
				kind: this.determineComposerKind(composer),
				description: this.getComposerDescription(composer),
				data: composer,
				composer,
				isGenerated: false,
			})
		}
	}

	private determineComposerKind(composer: any): TypeKind {
		for (const [ComposerClass, kind] of this.composerToKindMap) {
			if (composer instanceof ComposerClass) {
				return kind
			}
		}
		return TypeKind.UNKNOWN
	}

	private getComposerDescription(composer: any): string | undefined {
		return typeof composer.getDescription === 'function' ? composer.getDescription() : undefined
	}

	registerEdgeType(name: string, composer: ObjectTypeComposer<any, any>): void {
		this.edgeTypes.add(name)
		this.registerType(name, TypeKind.EDGE, composer, true)
	}

	hasEdgeType(name: string): boolean {
		return this.edgeTypes.has(name) || this.isTypeOfKind(name, TypeKind.EDGE)
	}

	getEdgeTypes(): string[] {
		return Array.from(this.edgeTypes)
	}

	private processedRelations: Set<string> = new Set()

	hasProcessedRelation(key: string): boolean {
		return this.processedRelations.has(key)
	}

	addProcessedRelation(key: string): void {
		this.processedRelations.add(key)
	}

	getProcessedRelations(): string[] {
		return Array.from(this.processedRelations)
	}

	isBuiltInScalar(typeName: string): boolean {
		const builtInScalars = ['String', 'Int', 'Float', 'Boolean', 'ID']
		return builtInScalars.includes(typeName)
	}
}
