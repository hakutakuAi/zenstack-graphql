import { PrismaClient } from '@prisma/client'
import type { GraphQLResolveInfo } from 'graphql'
import { ConnectionBuilder, PaginationArgs, ConnectionResult } from '../../schema-helpers'

export abstract class BaseResolver {
	protected async findMany<T>(
		prisma: PrismaClient,
		model: string,
		options: {
			where?: any
			orderBy?: any
			include?: any
		} = {},
	): Promise<T[]> {
		const modelDelegate = (prisma as any)[model]
		return (await modelDelegate.findMany({
			...options,
		})) as T[]
	}

	protected async findUnique<T>(prisma: PrismaClient, model: string, where: any, include?: any): Promise<T | null> {
		const modelDelegate = (prisma as any)[model]
		return (await modelDelegate.findUnique({
			where,
			include,
		})) as T | null
	}

	protected async create<T>(prisma: PrismaClient, model: string, data: any, include?: any): Promise<T> {
		const modelDelegate = (prisma as any)[model]
		return (await modelDelegate.create({
			data,
			include,
		})) as T
	}

	protected async update<T>(prisma: PrismaClient, model: string, where: any, data: any, include?: any): Promise<T | null> {
		const modelDelegate = (prisma as any)[model]
		return (await modelDelegate.update({
			where,
			data,
			include,
		})) as T | null
	}

	protected async buildConnection<T>(
		prisma: PrismaClient,
		model: string,
		pagination: PaginationArgs,
		options: {
			where?: any
			orderBy?: any
			include?: any
			info?: GraphQLResolveInfo
			relationFields?: string[]
			cursorField?: string
			hasIdField?: boolean
		} = {},
	): Promise<ConnectionResult<T>> {
		return ConnectionBuilder.build<T>({
			prisma,
			model,
			pagination,
			...options,
		})
	}
}
