import { SchemaComposer, ObjectTypeComposer, EnumTypeComposer, ScalarTypeComposer, InputTypeComposer } from 'graphql-compose'
import { printSchema, GraphQLSchema } from 'graphql'
import { ErrorHandler, ErrorCategory } from '@utils/error/error-handler'
import { HandleErrors, SchemaOp, Generate } from '@utils/error'
import { TypeFormatter } from '@utils/schema/type-formatter'

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

export interface TypeInfo {
	name: string
	kind: TypeKind
	description?: string
	composer: any
	isGenerated: boolean
}

export class UnifiedRegistry {
	private types: Map<string, TypeInfo> = new Map()
	private readonly schemaComposer: SchemaComposer<unknown>
	private readonly errorHandler: ErrorHandler
	private processedRelations: Set<string> = new Set()
	private edgeTypes: Set<string> = new Set()

	constructor(schemaComposer: SchemaComposer<unknown>, errorHandler: ErrorHandler = new ErrorHandler()) {
		this.schemaComposer = schemaComposer
		this.errorHandler = errorHandler
		this.syncFromSchemaComposer()
	}

	@SchemaOp()
	private syncFromSchemaComposer(): void {
		for (const typeName of this.schemaComposer.types.keys()) {
			const composer = this.schemaComposer.get(typeName)
			if (!composer) continue

			let kind = TypeKind.UNKNOWN
			if (composer instanceof ObjectTypeComposer) kind = TypeKind.OBJECT
			if (composer instanceof ScalarTypeComposer) kind = TypeKind.SCALAR
			if (composer instanceof EnumTypeComposer) kind = TypeKind.ENUM

			this.types.set(typeName, {
				name: typeName,
				kind,
				description: this.getComposerDescription(composer),
				composer,
				isGenerated: false,
			})
		}
	}

	@SchemaOp()
	registerType(typeName: string, kind: TypeKind, composer: any, isGenerated = true): void {
		if (this.types.has(typeName)) {
			const existing = this.types.get(typeName)!
			if (existing.kind !== kind) {
				this.errorHandler.logWarning(`Type ${typeName} already exists with kind ${existing.kind}, but trying to register as ${kind}`, ErrorCategory.SCHEMA, {
					typeName,
					existingKind: existing.kind,
					newKind: kind,
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

	@SchemaOp()
	getTypeComposer<T = any>(typeName: string): T | undefined {
		const typeInfo = this.types.get(typeName)
		if (!typeInfo) {
			if (this.schemaComposer.has(typeName)) {
				return this.schemaComposer.get(typeName) as T
			}
			return undefined
		}
		return typeInfo.composer as T
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
		if (!this.schemaComposer.has(name)) {
			return undefined
		}

		const composer = this.schemaComposer.get(name)
		return composer instanceof ObjectTypeComposer ? composer : undefined
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
		if (!this.schemaComposer.has(name)) {
			return undefined
		}

		const composer = this.schemaComposer.get(name)
		return composer instanceof EnumTypeComposer ? composer : undefined
	}

	getEnumValues(enumName: string): string[] {
		const enumComposer = this.getEnumComposer(enumName)
		return enumComposer ? Object.keys(enumComposer.getFields()) : []
	}

	isValidEnumValue(enumName: string, value: string): boolean {
		return this.getEnumValues(enumName).includes(value)
	}

	getScalarComposer(name: string): ScalarTypeComposer<any> | undefined {
		if (!this.schemaComposer.has(name)) {
			return undefined
		}

		const composer = this.schemaComposer.get(name)
		return composer instanceof ScalarTypeComposer ? composer : undefined
	}

	isBuiltInScalar(typeName: string): boolean {
		const builtInScalars = ['String', 'Int', 'Float', 'Boolean', 'ID']
		return builtInScalars.includes(typeName)
	}

	registerEdgeType(name: string, composer: any): void {
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

	@SchemaOp()
	validateSchema(): string[] {
		try {
			this.schemaComposer.buildSchema()
			return []
		} catch (error) {
			return [`Schema validation failed: ${error instanceof Error ? error.message : String(error)}`]
		}
	}

	@SchemaOp()
	buildSchema(): GraphQLSchema {
		return this.schemaComposer.buildSchema({ keepUnusedTypes: true })
	}

	@Generate()
	generateSDL(): string {
		const schema = this.buildSchema()
		return printSchema(schema)
	}

	@SchemaOp()
	addRelayRequirements(): void {
		if (this.schemaComposer.has('Node')) {
			return
		}

		const nodeInterface = this.schemaComposer.createInterfaceTC({
			name: 'Node',
			description: 'An object with a unique identifier',
			fields: {
				id: {
					type: 'ID!',
					description: 'The unique identifier for this object',
				},
			},
		})

		this.registerType('Node', TypeKind.INTERFACE, nodeInterface, true)
	}
}
