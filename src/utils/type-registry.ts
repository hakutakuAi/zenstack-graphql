import { SchemaComposer, ObjectTypeComposer, ScalarTypeComposer, EnumTypeComposer } from 'graphql-compose'
import { ErrorHandler, ErrorCategory } from './error-handler'

export enum TypeKind {
	OBJECT = 'object',
	SCALAR = 'scalar',
	ENUM = 'enum',
	INTERFACE = 'interface',
	UNION = 'union',
	INPUT = 'input',
	CONNECTION = 'conne	ction',
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

export class TypeRegistry {
	private types: Map<string, TypeInfo> = new Map()
	private readonly schemaComposer: SchemaComposer<unknown>
	private readonly errorHandler: ErrorHandler

	constructor(schemaComposer: SchemaComposer<unknown>, errorHandler: ErrorHandler) {
		this.schemaComposer = schemaComposer
		this.errorHandler = errorHandler
		this.syncFromSchemaComposer()
	}

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

	registerType(typeName: string, kind: TypeKind, composer: any, isGenerated = true): void {
		try {
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
		} catch (error) {
			this.errorHandler.logWarning(`Failed to register type ${typeName}`, ErrorCategory.SCHEMA, { typeName, kind, error })
		}
	}

	hasType(typeName: string): boolean {
		return this.types.has(typeName) || this.schemaComposer.has(typeName)
	}

	isTypeOfKind(typeName: string, kind: TypeKind): boolean {
		const typeInfo = this.types.get(typeName)
		return typeInfo ? typeInfo.kind === kind : false
	}

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
}
