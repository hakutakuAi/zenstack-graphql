import { SchemaComposer, ObjectTypeComposer, ScalarTypeComposer, EnumTypeComposer } from 'graphql-compose'
import { printSchema, GraphQLSchema } from 'graphql'
import { ErrorHandler, ErrorCategory } from '@utils/error/error-handler'
import { SchemaOp, Generate } from '@utils/error'

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

export class SchemaRegistry {
	private types: Map<string, TypeInfo> = new Map()
	private readonly schemaComposer: SchemaComposer<unknown>
	private readonly errorHandler: ErrorHandler

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
				description: composer.getDescription(),
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
			description: composer.getDescription(),
			composer,
			isGenerated,
		})

		this.schemaComposer.set(typeName, composer)
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

	getEdgeTypes(): string[] {
		return this.getTypesByKind(TypeKind.EDGE)
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
