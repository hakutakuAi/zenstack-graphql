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
import { ErrorCategory, PluginError, warning } from '@utils/error'
import { TypeFormatter } from '@utils/schema/type-formatter'
import { BUILTIN_SCALARS, COMMON_TYPES } from '@utils/constants'

export enum TypeKind {
	OBJECT = 'object',
	SCALAR = 'scalar',
	ENUM = 'enum',
	INTERFACE = 'interface',
	UNION = 'union',
	INPUT = 'input',
	CONNECTION = 'connection',
	EDGE = 'edge',
	UNKNOWN = 'unknown',
}

type ComposerTypeMap = {
	[TypeKind.OBJECT]: ObjectTypeComposer<any, any>
	[TypeKind.SCALAR]: ScalarTypeComposer<any>
	[TypeKind.ENUM]: EnumTypeComposer<any>
	[TypeKind.INTERFACE]: InterfaceTypeComposer<any>
	[TypeKind.UNION]: UnionTypeComposer<any>
	[TypeKind.INPUT]: InputTypeComposer<any>
	[TypeKind.CONNECTION]: ObjectTypeComposer<any, any>
	[TypeKind.EDGE]: ObjectTypeComposer<any, any>
	[TypeKind.UNKNOWN]: any
}

export interface TypeInfo {
	name: string
	kind: TypeKind
	description?: string
	composer: any
	isGenerated: boolean
}

export class Registry {
	private types: Map<string, TypeInfo> = new Map()
	private readonly schemaComposer: SchemaComposer<unknown>
	private processedRelations: Set<string> = new Set()
	private edgeTypes: Set<string> = new Set()
	private readonly composerToKindMap = new Map<any, TypeKind>([
		[ObjectTypeComposer, TypeKind.OBJECT],
		[ScalarTypeComposer, TypeKind.SCALAR],
		[EnumTypeComposer, TypeKind.ENUM],
		[InterfaceTypeComposer, TypeKind.INTERFACE],
		[InputTypeComposer, TypeKind.INPUT],
		[UnionTypeComposer, TypeKind.UNION]
	])

	constructor(schemaComposer: SchemaComposer<unknown>) {
		this.schemaComposer = schemaComposer
		this.syncFromSchemaComposer()
	}

	private syncFromSchemaComposer(): void {
		for (const typeName of this.schemaComposer.types.keys()) {
			const composer = this.schemaComposer.get(typeName)
			if (!composer) continue

			this.types.set(typeName, {
				name: typeName,
				kind: this.determineComposerKind(composer),
				description: this.getComposerDescription(composer),
				composer,
				isGenerated: false,
			})
		}
	}

	registerType<K extends keyof ComposerTypeMap>(typeName: string, kind: K, composer: ComposerTypeMap[K], isGenerated = true): void {
		if (this.types.has(typeName)) {
			const existing = this.types.get(typeName)!
			if (existing.kind !== kind) {
				warning(`Type ${typeName} already exists with kind ${existing.kind}, but trying to register as ${kind}`, ErrorCategory.SCHEMA, {
					typeName,
					existingKind: existing.kind,
					newKind: kind,
					conflictType: 'TypeKindMismatch',
				})
			}
			return
		}

		this.types.set(typeName, {
			name: typeName,
			kind,
			description: this.getComposerDescription(composer),
			composer,
			isGenerated,
		})

		this.schemaComposer.set(typeName, composer)
	}

	private getComposerDescription(composer: any): string | undefined {
		return typeof composer.getDescription === 'function' ? composer.getDescription() : undefined
	}

	hasType(typeName: string): boolean {
		return this.types.has(typeName) || this.schemaComposer.has(typeName)
	}

	isTypeOfKind(typeName: string, kind: TypeKind): boolean {
		const typeInfo = this.types.get(typeName)
		return typeInfo ? typeInfo.kind === kind : false
	}

	getTypeComposer<K extends keyof ComposerTypeMap>(typeName: string, kind: K): ComposerTypeMap[K] | undefined {
		const typeInfo = this.types.get(typeName)

		if (!typeInfo) {
			if (this.schemaComposer.has(typeName)) {
				const composer = this.schemaComposer.get(typeName)
				if (this.isComposerOfKind(composer, kind)) {
					return composer as ComposerTypeMap[K]
				} else {
					const actualKind = this.determineComposerKind(composer)
					warning(`Type ${typeName} exists but is of kind ${actualKind}, not ${kind}`, ErrorCategory.SCHEMA, {
						typeName,
						requestedKind: kind,
						actualKind,
						errorType: 'TypeKindMismatch',
					})
				}
			}
			return undefined
		}

		if (typeInfo.kind !== kind) {
			warning(`Type ${typeName} is registered with kind ${typeInfo.kind}, not ${kind}`, ErrorCategory.SCHEMA, {
				typeName,
				requestedKind: kind,
				actualKind: typeInfo.kind,
				errorType: 'RegisteredTypeMismatch',
			})
			return undefined
		}

		return typeInfo.composer as ComposerTypeMap[K]
	}

	private determineComposerKind(composer: any): TypeKind {
		for (const [ComposerClass, kind] of this.composerToKindMap) {
			if (composer instanceof ComposerClass) return kind
		}
		return TypeKind.UNKNOWN
	}

	private isComposerOfKind(composer: any, kind: TypeKind): boolean {
		if (!composer) return false
		
		if (kind === TypeKind.CONNECTION || kind === TypeKind.EDGE) {
			return composer instanceof ObjectTypeComposer
		}
		
		return this.determineComposerKind(composer) === kind
	}

	getTypesByKind(kind: TypeKind): string[] {
		return Array.from(this.types.values())
			.filter((info) => info.kind === kind)
			.map((info) => info.name)
	}

	getGeneratedTypes(): string[] {
		return Array.from(this.types.values())
			.filter((info) => info.isGenerated)
			.map((info) => info.name)
	}

	getObjectComposer(name: string): ObjectTypeComposer<any, any> | undefined {
		return this.getTypeComposer(name, TypeKind.OBJECT)
	}

	getObjectTypeFields(typeName: string): string[] {
		const objectComposer = this.getObjectComposer(typeName)
		return objectComposer ? Object.keys(objectComposer.getFields()) : []
	}

	hasField(typeName: string, fieldName: string): boolean {
		return this.getObjectTypeFields(typeName).includes(fieldName)
	}

	getFieldType(typeName: string, fieldName: string): string | undefined {
		const objectComposer = this.getObjectComposer(typeName)
		if (!objectComposer) {
			return undefined
		}

		const field = objectComposer.getField(fieldName)
		return field?.type?.toString()
	}

	getEnumComposer(name: string): EnumTypeComposer<any> | undefined {
		return this.getTypeComposer(name, TypeKind.ENUM)
	}

	getEnumValues(enumName: string): string[] {
		const enumComposer = this.getEnumComposer(enumName)
		return enumComposer ? Object.keys(enumComposer.getFields()) : []
	}

	isValidEnumValue(enumName: string, value: string): boolean {
		return this.getEnumValues(enumName).includes(value)
	}

	getScalarComposer(name: string): ScalarTypeComposer<any> | undefined {
		return this.getTypeComposer(name, TypeKind.SCALAR)
	}

	getInputComposer(name: string): InputTypeComposer<any> | undefined {
		return this.getTypeComposer(name, TypeKind.INPUT)
	}

	getInterfaceComposer(name: string): InterfaceTypeComposer<any> | undefined {
		return this.getTypeComposer(name, TypeKind.INTERFACE)
	}

	isBuiltInScalar(typeName: string): boolean {
		return BUILTIN_SCALARS.includes(typeName as any)
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

	getConnectionTypeForModel(modelName: string, typeFormatter: TypeFormatter): string | undefined {
		const connectionName = typeFormatter.formatConnectionTypeName(modelName)
		return this.hasType(connectionName) ? connectionName : undefined
	}

	hasProcessedRelation(key: string): boolean {
		return this.processedRelations.has(key)
	}

	addProcessedRelation(key: string): void {
		this.processedRelations.add(key)
	}

	getProcessedRelations(): string[] {
		return Array.from(this.processedRelations)
	}

	getObjectTypes(): string[] {
		return this.getTypesByKind(TypeKind.OBJECT)
	}

	getScalarTypes(): string[] {
		return this.getTypesByKind(TypeKind.SCALAR)
	}

	getEnumTypes(): string[] {
		return this.getTypesByKind(TypeKind.ENUM)
	}

	getConnectionTypes(): string[] {
		return this.getTypesByKind(TypeKind.CONNECTION)
	}

	getInputTypes(): string[] {
		return this.getTypesByKind(TypeKind.INPUT)
	}

	validateSchema(): string[] {
		try {
			this.schemaComposer.buildSchema({
				keepUnusedTypes: true,
			})
			return []
		} catch (error) {
			const errorMessage = `Schema validation failed: ${error instanceof Error ? error.message : String(error)}`

			warning(errorMessage, ErrorCategory.SCHEMA, {
				validationError: error,
				errorType: 'SchemaValidation',
			})

			return [errorMessage]
		}
	}

	buildSchema(): GraphQLSchema {
		try {
			return this.schemaComposer.buildSchema({ keepUnusedTypes: true })
		} catch (error) {
			throw new PluginError(`Failed to build GraphQL schema`, ErrorCategory.SCHEMA, { originalError: error }, [
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
}
