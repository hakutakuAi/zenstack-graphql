import { ErrorCategory, warning } from '@utils/error'

export enum TypeKind {
	OBJECT = 'object',
	SCALAR = 'scalar',
	ENUM = 'enum',
	INTERFACE = 'interface',
	UNION = 'union',
	INPUT = 'input',
	CONNECTION = 'connection',
	EDGE = 'edge',
	RESOLVER = 'resolver',
	UNKNOWN = 'unknown',
}

export interface BaseTypeInfo<TData = any> {
	name: string
	kind: TypeKind
	description?: string
	data: TData
	isGenerated: boolean
	dependencies?: string[]
}

export interface ITypeRegistry<TData = any, TInfo extends BaseTypeInfo<TData> = BaseTypeInfo<TData>> {
	registerType(name: string, kind: TypeKind, data: TData, isGenerated?: boolean): void
	hasType(typeName: string): boolean
	getType(typeName: string): TInfo | undefined
	getTypesByKind(kind: TypeKind): TInfo[]
	getGeneratedTypes(): TInfo[]
	isTypeOfKind(typeName: string, kind: TypeKind): boolean
	validateSchema(): string[]
}

export abstract class BaseRegistry<TData, TInfo extends BaseTypeInfo<TData>> implements ITypeRegistry<TData, TInfo> {
	protected types: Map<string, TInfo> = new Map()
	protected processedItems: Set<string> = new Set()

	registerType(name: string, kind: TypeKind, data: TData, isGenerated = true): void {
		if (this.types.has(name)) {
			const existing = this.types.get(name)!
			if (existing.kind !== kind) {
				warning(`Type ${name} already exists with kind ${existing.kind}, but trying to register as ${kind}`, ErrorCategory.SCHEMA, {
					typeName: name,
					existingKind: existing.kind,
					newKind: kind,
					conflictType: 'TypeKindMismatch',
				})
			}
			return
		}

		this.types.set(name, this.createTypeInfo(name, kind, data, isGenerated))
	}

	hasType(typeName: string): boolean {
		return this.types.has(typeName)
	}

	getType(typeName: string): TInfo | undefined {
		return this.types.get(typeName)
	}

	getTypesByKind(kind: TypeKind): TInfo[] {
		return Array.from(this.types.values()).filter((info) => info.kind === kind)
	}

	getGeneratedTypes(): TInfo[] {
		return Array.from(this.types.values()).filter((info) => info.isGenerated)
	}

	isTypeOfKind(typeName: string, kind: TypeKind): boolean {
		const typeInfo = this.types.get(typeName)
		return typeInfo ? typeInfo.kind === kind : false
	}

	protected abstract createTypeInfo(name: string, kind: TypeKind, data: TData, isGenerated: boolean): TInfo

	abstract validateSchema(): string[]

	protected processItem(itemName: string, processor: () => void): void {
		if (this.processedItems.has(itemName)) {
			return
		}

		this.processedItems.add(itemName)
		processor()
	}

	protected getTypeNames(): string[] {
		return Array.from(this.types.keys())
	}

	protected getAllTypes(): TInfo[] {
		return Array.from(this.types.values())
	}
}
